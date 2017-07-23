{-# LANGUAGE OverloadedStrings #-}

module IRTS.CodegenGo (codegenGo) where

import           Control.Applicative       ((<|>))
import           Control.Monad.Trans.State (State (..), evalState, gets)
import           Data.Char                 (isAlphaNum, ord)
import           Data.Int                  (Int64)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe, mapMaybe)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Formatting                (int, sformat, stext, string, (%))
import           System.IO                 (IOMode (..), withFile)
import           System.Process            (CreateProcess (..), StdStream (..),
                                            createProcess, proc, waitForProcess)

import           Idris.Core.TT             hiding (V, arity)
import           IRTS.CodegenCommon
import           IRTS.Lang                 (FDesc (..), FType (..), LVar (..),
                                            PrimFn (..))
import           IRTS.Simplified


data Line = Line (Maybe Var) [Var] T.Text
  deriving (Show)

data Var = RVal | V Int
   deriving (Show, Eq, Ord)

newtype CGState = CGState { requiresTrampoline :: Name -> Bool
                          }

type CG a = State CGState a

createCgState :: (Name -> Bool) -> CGState
createCgState trampolineLookup = CGState { requiresTrampoline = trampolineLookup }

goPreamble :: [T.Text] -> T.Text
goPreamble imports = T.unlines $
  [ "// THIS FILE IS AUTOGENERATED! DO NOT EDIT"
  , ""
  , "package main"
  , ""
  , "import \"math/big\""
  , "import \"os\""
  , "import \"strconv\""
  , "import \"unicode/utf8\""
  , "import \"unsafe\""
  , ""
  ] ++ map ("import " `T.append`) imports ++
  [ ""
  , "func BigIntFromString(s string) *big.Int {"
  , "  value, _ := big.NewInt(0).SetString(s, 10)"
  , "  return value"
  , "}"
  , ""
  , "type Con struct {"
  , "  tag int"
  , "  args []unsafe.Pointer"
  , "}"
  , ""
  , "func GetTag(con unsafe.Pointer) int64 {"
  , "  return int64((*Con)(con).tag)"
  , "}"
  , ""
  , "func MakeCon(tag int, args ...unsafe.Pointer) unsafe.Pointer {"
  , "  return unsafe.Pointer(&Con{tag, args})"
  , "}"
  , ""
  , "func MkIntFromBool(value bool) unsafe.Pointer {"
  , "  var retVal *int64 = new(int64)"
  , "  if value {"
  , "     *retVal = 1"
  , "  } else {"
  , "     *retVal = 0"
  , "  }"
  , "  return unsafe.Pointer(retVal)"
  , "}"
  , ""
  , "func MkInt(value int64) unsafe.Pointer {"
  , "  var retVal *int64 = new(int64)"
  , "  *retVal = value"
  , "  return unsafe.Pointer(retVal)"
  , "}"
  , ""
  , "func MkRune(value rune) unsafe.Pointer {"
  , "  var retVal *rune = new(rune)"
  , "  *retVal = value"
  , "  return unsafe.Pointer(retVal)"
  , "}"
  , ""
  , "func MkString(value string) unsafe.Pointer {"
  , "  var retVal *string = new(string)"
  , "  *retVal = value"
  , "  return unsafe.Pointer(retVal)"
  , "}"
  , ""
  , "func RuneAtIndex(s string, index int) rune {"
  , "  if index == 0 {"
  , "   chr, _ := utf8.DecodeRuneInString(s)"
  , "    return chr"
  , "  } else {"
  , "    i := 0"
  , "    for _, chr := range s {"
  , "      if i == index {"
  , "        return chr"
  , "      }"
  , "      i++"
  , "    }"
  , "  }"
  , "panic(\"Illegal index: \" + string(index))"
  , "}"
  , ""
  , "func StrTail(s string) string {"
  , "  _, offset := utf8.DecodeRuneInString(s)"
  , "  return s[offset:]"
  , "}"
  , ""
  , "func WriteStr(str unsafe.Pointer) unsafe.Pointer {"
  , "  _, err := os.Stdout.WriteString(*(*string)(str))"
  , "  if (err != nil) {"
  , "    return MkInt(0)"
  , "  } else {"
  , "    return MkInt(-1)"
  , "  }"
  , "}"
  , ""
  , "func Go(action unsafe.Pointer) {"
  , "  go Trampoline(func () (thunk, unsafe.Pointer) { return APPLY0(action, nil) })"
  , "}"
  , ""
  , "func MkMaybe(value unsafe.Pointer, present bool) unsafe.Pointer {"
  , "  if present {"
  , "    return MakeCon(1, value)"
  , "  } else {"
  , "    return MakeCon(0)"
  , "  }"
  , "}"
  , ""
  , "type thunk func() (thunk, unsafe.Pointer)"
  , ""
  , "func Trampoline(f thunk) unsafe.Pointer {"
  , "  var result unsafe.Pointer"
  , "  for {"
  , "    f, result = f()"
  , "    if f == nil {"
  , "      return result"
  , "    }"
  , "  }"
  , "}"
  , ""
  -- This solely exists so the strconv import is used even if the program
  -- doesn't use the LIntStr primitive.
  , "func __useStrconvImport() string {"
  , "  return strconv.Itoa(-42)"
  , "}"
  , ""
  ]


mangleName :: Name -> T.Text
mangleName name = T.concat $ map mangleChar (showCG name)
  where
    mangleChar x
      | isAlphaNum x = T.singleton x
      | otherwise = sformat ("_" % int % "_") (ord x)

nameToGo :: Name -> T.Text
nameToGo (MN i n) | T.all (\x -> isAlphaNum x || x == '_') n =
                    n `T.append` T.pack (show i)
nameToGo n = mangleName n

lVarToGo :: LVar -> T.Text
lVarToGo (Loc i)  = sformat ("_" % int) i
lVarToGo (Glob n) = nameToGo n

lVarToVar :: LVar -> Var
lVarToVar (Loc i) = V i
lVarToVar v       = error $ "LVar not convertible to var: " ++ show v

varToGo :: Var -> T.Text
varToGo RVal  = "__rval"
varToGo (V i) = sformat ("_" % int) i

exprToGo :: Name -> Var -> SExp -> CG [Line]

exprToGo f var SNothing = return . return $
  Line (Just var) [] ("    " `T.append` varToGo var `T.append` " = nil")

exprToGo f var (SConst i@BI{}) = return
  [ Line (Just var) [] (sformat ("  " % stext % " = unsafe.Pointer(" % stext % ")") (varToGo var) (constToGo i)) ]
exprToGo f var (SConst c@Ch{}) = return . return $ mkVal var c (sformat ("MkRune(" % stext % ")"))
exprToGo f var (SConst i@I{}) = return . return $ mkVal var i (sformat ("MkInt(" % stext % ")"))
exprToGo f var (SConst s@Str{}) = return . return $ mkVal var s (sformat ("MkString(" % stext % ")"))

exprToGo f var (SV (Loc i)) = return
  [ Line (Just var) [V i] ("    " `T.append` varToGo var `T.append` " = " `T.append` lVarToGo (Loc i)) ]

exprToGo f var (SLet (Loc i) e sc) = do
  a <- exprToGo f (V i) e
  b <- exprToGo f var sc
  return $ a ++ b

exprToGo f var (SApp True name vs)
  -- self call, simply goto to the entry again
  | f == name = return $
    [ Line (Just (V i)) [ V a ] (sformat ("_" % int % " = _" % int) i a) | (i, Loc a) <- zip [0..] vs ] ++
    [ Line Nothing [ ] "goto entry" ]
exprToGo f RVal (SApp True name vs) = do
  let args = T.intercalate ", " (map lVarToGo vs)
      call = nameToGo name `T.append` "(" `T.append` args `T.append` ")"
  trampolined <- fmap ($ name) (gets requiresTrampoline)
  let code = if trampolined
        then "__thunk = func() (thunk, unsafe.Pointer) { return " `T.append` call `T.append` " }"
        else "__rval = " `T.append` call
  return  [ Line (Just RVal) [ V i | (Loc i) <- vs ] code ]
exprToGo _ var (SApp True _ _) = error $ "Tail-recursive call, but should be assigned to " ++ show var
exprToGo f var (SApp False name vs) = do
  -- Not a tail call, but we might call a function that needs to be trampolined
  trampolined <- fmap ($ name) (gets requiresTrampoline)
  let call = sformat (stext % "(" % stext % ")") (nameToGo name) args
      decorated = if trampolined
                  then sformat ("Trampoline(func () (thunk, unsafe.Pointer) { return " % stext % "})") call
                  else call
  return  [ Line (Just var) [ V i | (Loc i) <- vs ]
            (sformat (stext % " = " % stext) (varToGo var) decorated)
          ]
  where
    args = T.intercalate ", " (map lVarToGo vs)

exprToGo f var (SCase up (Loc l) alts)
   | isBigIntConst alts = constBigIntCase f var (V l) (dedupDefaults alts)
   | isConst alts = constCase f var (V l) alts
   | otherwise = conCase f var (V l) alts
  where
    isBigIntConst (SConstCase (BI _) _ : _) = True
    isBigIntConst _                         = False

    isConst []                   = False
    isConst (SConstCase _ _ : _) = True
    isConst (SConCase{} : _)     = False
    isConst (_ : _)              = False

    dedupDefaults (d@SDefaultCase{} : [SDefaultCase{}]) = [d]
    dedupDefaults (x : xs)                              = x : dedupDefaults xs
    dedupDefaults []                                    = []

exprToGo f var (SChkCase (Loc l) alts) = conCase f var (V l) alts

exprToGo f var (SCon rVar tag name args) = return . return $
  Line (Just var)  [ V i | (Loc i) <- args]
  (sformat (stext % " = MakeCon(" % int % stext % ")") (varToGo var) tag argsCode)
  where
    argsCode = case args of
      [] -> T.empty
      _  -> ", " `T.append` T.intercalate ", " (map lVarToGo args)

exprToGo f var (SOp prim args) = return . return $ primToGo var prim args

exprToGo f var (SForeign ty (FApp callType callTypeArgs) args) =
  let call = toCall callType callTypeArgs
  in return . return $ Line Nothing [] (retVal (fDescToGoType ty) call)
  where
    convertedArgs = [ toArg (fDescToGoType t) (lVarToGo l) | (t, l) <- args]

    toCall ct [ FStr fname ]
      | ct == sUN "Function" = T.pack fname `T.append` "(" `T.append` T.intercalate ", " convertedArgs `T.append` ")"
    toCall ct [ FStr _, _, FStr methodName ]
      | ct == sUN "Method" =
        let obj : args = convertedArgs in
          sformat (stext % "." % string % "(" % stext % ")")
        obj methodName (T.intercalate ", " args)
    toCall ct a = error $ show ct ++ " " ++ show a

    toArg (GoInterface name) x = sformat ("(*(*" % string % ")(" % stext % "))") name x
    toArg GoByte x = "byte(*(*rune)(" `T.append` x `T.append` "))"
    toArg GoString x = "*(*string)(" `T.append` x `T.append` ")"
    toArg GoAny x = x
    toArg f _ = error $ "Not implemented yet: toArg " ++ show f

    ptrFromRef x = "unsafe.Pointer(&" `T.append` x `T.append` ")"
    toPtr (GoInterface _) x = ptrFromRef x
    toPtr GoInt x = ptrFromRef x
    toPtr GoString x = ptrFromRef x
    toPtr (GoNilable valueType) x =
      sformat ("MkMaybe(" % stext % ", " % stext % " != nil)" )
      (toPtr valueType x) x
    retRef ty x =
      sformat ("{ __tmp := " % stext % "\n " % stext % " = " % stext % " }")
      x (varToGo var) (toPtr ty "__tmp")

    retVal GoUnit x = x
    retVal GoString x = retRef GoString x
    retVal (i@GoInterface{}) x = retRef i x
    retVal (n@GoNilable{}) x = retRef n x
    retVal (GoMultiVal varTypes) x =
      -- XXX assumes exactly two vars
      sformat ("{ " % stext % " := " % stext % "\n " % stext % " = MakeCon(0, " % stext % ") }")
      (T.intercalate ", " [ sformat ("__tmp" % int) i | i <- [1..length varTypes]])
      x
      (varToGo var)
      (T.intercalate ", " [ toPtr varTy (sformat ("__tmp" % int) i) | (i, varTy) <- zip [1..] varTypes ])
    retVal (GoPtr _) x = sformat (stext % " = unsafe.Pointer(" % stext % ")") (varToGo var) x
    retVal t _ = error $ "Not implemented yet: retVal " ++ show t

exprToGo _ _ expr = error $ "Not implemented yet: " ++ show expr


data GoType = GoByte
            | GoInt
            | GoString
            | GoNilable GoType
            | GoInterface String
            | GoUnit
            | GoMultiVal [GoType]
            | GoPtr GoType
            | GoAny
  deriving (Show)

fDescToGoType :: FDesc -> GoType
fDescToGoType (FCon c)
  | c == sUN "Go_Byte" = GoByte
  | c == sUN "Go_Int" = GoInt
  | c == sUN "Go_Str" = GoString
  | c == sUN "Go_Unit" = GoUnit
fDescToGoType (FApp c [ FStr name ])
  | c == sUN "Go_Interface" = GoInterface name
fDescToGoType (FApp c [ _ ])
  | c == sUN "Go_Any" = GoAny
fDescToGoType (FApp c [ _, ty ])
  | c == sUN "Go_Nilable" = GoNilable (fDescToGoType ty)
fDescToGoType (FApp c [ _, _, FApp c2 [ _, _, a, b ] ])
  | c == sUN "Go_MultiVal" && c2 == sUN "MkPair" = GoMultiVal [ fDescToGoType a, fDescToGoType b ]
fDescToGoType (FApp c [ _, ty ])
  | c == sUN "Go_Ptr" = GoPtr (fDescToGoType ty)
fDescToGoType f = error $ "Not implemented yet: fDescToGoType " ++ show f


toFunType :: FDesc -> FType
toFunType (FApp c [ _, _ ])
    | c == sUN "Go_FnBase" = FFunction
    | c == sUN "Go_FnIO" = FFunctionIO
toFunType desc = error $ "Not implemented yet: toFunType " ++ show desc

mkVal :: Var -> Const -> (T.Text -> T.Text) -> Line
mkVal var c factory =
  Line (Just var) [] (sformat ("  " % stext % " = " % stext) (varToGo var) (factory (constToGo c)))

constToGo :: Const -> T.Text
constToGo (BI i) = if i < toInteger (maxBound :: Int64) && i > toInteger (minBound :: Int64)
  then "big.NewInt(" `T.append` T.pack (show i) `T.append` ")"
  else "BigIntFromString(\"" `T.append` T.pack (show i) `T.append` "\")"
constToGo (Ch '\DEL') = "'\\x7F'"
constToGo (Ch '\SO') = "'\\x0e'"
constToGo (Str s) = T.pack (show s)
constToGo constVal = T.pack (show constVal)

-- Special case for big.Ints, as we need to compare with Cmp there
constBigIntCase :: Name -> Var -> Var -> [SAlt] -> CG [Line]
constBigIntCase f var v alts = do
  cases <- traverse case_ alts
  return $
    [ Line Nothing [] "switch {" ] ++ concat cases ++ [ Line Nothing [] "}" ]
  where
    valueCmp other = sformat ("(*big.Int)(" % stext % ").Cmp(" % stext % ") == 0") (varToGo v) (constToGo other)
    case_ (SConstCase constVal expr) = do
      code <- exprToGo f var expr
      return $ Line Nothing [v] (sformat ("case " % stext % ":") (valueCmp constVal)) : code
    case_ (SDefaultCase expr) = do
      code <- exprToGo f var expr
      return $ Line Nothing [] "default:" : code
    case_ c = error $ "Unexpected big int case: " ++ show c

constCase :: Name -> Var -> Var -> [SAlt] -> CG [Line]
constCase f var v alts = do
  cases <- traverse case_ alts
  return $ [ Line Nothing [v] (T.concat [ "switch " , castValue alts , " {" ])
           ] ++ concat cases ++ [ Line Nothing [] "}" ]
  where
    castValue (SConstCase (Ch _) _ : _) = "*(*rune)(" `T.append` varToGo v `T.append` ")"
    castValue (SConstCase (I _) _ : _) = "*(*int64)(" `T.append` varToGo v `T.append` ")"
    castValue (SConstCase constVal _ : _) = error $ "Not implemented: cast for " ++ show constVal
    castValue _ = error "First alt not a SConstCase!"

    case_ (SDefaultCase expr) = do
      code <- exprToGo f var expr
      return $ Line Nothing [] "default:" : code
    case_ (SConstCase constVal expr) = do
      code <- exprToGo f var expr
      return $
        Line Nothing [] (T.concat [ "case " , constToGo constVal , ":" ]) : code
    case_ c = error $ "Unexpected const case: " ++ show c


conCase :: Name -> Var -> Var -> [SAlt] -> CG [Line]
conCase f var v [ SDefaultCase expr ] = exprToGo f var expr
conCase f var v alts = do
  cases <- traverse case_ alts
  return $ [ Line Nothing [v] (T.concat [ "switch GetTag(" , varToGo v , ") {" ])
           ] ++ concat cases ++ [ Line Nothing [] "}" ]
  where
    project left i =
      Line (Just left) [v]
      (sformat (stext % " = (*Con)(" % stext % ").args[" % int % "]") (varToGo left) (varToGo v) i)
    case_ (SConCase base tag name args expr) = do
      let locals = [base .. base + length args - 1]
          projections = [ project (V i) (i - base) | i <- locals ]
      code <- exprToGo f var expr
      return $ [ Line Nothing [] (sformat ("case " % int % ":\n  // Projection of " % stext) tag (nameToGo name))
               ] ++ projections ++ code
    case_ (SDefaultCase expr) = do
      code <- exprToGo f var expr
      return $ Line Nothing [] "default:" : code
    case_ c = error $ "Unexpected con case: " ++ show c


primToGo :: Var -> PrimFn -> [LVar] -> Line
primToGo var (LChInt ITNative) [ch] =
  let code = T.concat [ varToGo var
                      , " = MkInt(int64(*(*rune)("
                      , lVarToGo ch
                      , ")))"
                      ]
  in Line (Just var) [ lVarToVar ch ] code
primToGo var (LEq (ATInt ITChar)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool(*(*rune)("
                       , lVarToGo left
                       , ") == *(*rune)("
                       , lVarToGo right
                       , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LEq (ATInt ITNative)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool(*(*int64)("
                       , lVarToGo left
                       , ") == *(*int64)("
                       , lVarToGo right
                       , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LEq (ATInt ITBig)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool((*big.Int)("
                       , lVarToGo left
                       , ").Cmp((*big.Int)("
                       , lVarToGo right
                       , ")) == 0)"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LSLt (ATInt ITChar)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool(*(*rune)("
                       , lVarToGo left
                       , ") < *(*rune)("
                       , lVarToGo right
                       , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LSLt (ATInt ITNative)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool(*(*int64)("
                       , lVarToGo left
                       , ") < *(*int64)("
                       , lVarToGo right
                       , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LSLt (ATInt ITBig)) [left, right] =
   let code = T.concat [ varToGo var
                       , " = MkIntFromBool((*big.Int)("
                       , lVarToGo left
                       , ").Cmp((*big.Int)("
                       , lVarToGo right
                       , ")) < 0)"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var (LMinus (ATInt ITNative)) [left, right] = nativeIntBinOp var left right "-"
primToGo var (LMinus (ATInt ITBig)) [left, right] = bigIntBigOp var left right "Sub"
primToGo var (LPlus (ATInt ITNative)) [left, right] = nativeIntBinOp var left right "+"
primToGo var (LPlus (ATInt ITBig)) [left, right] = bigIntBigOp var left right "Add"
primToGo var (LSExt ITNative ITBig) [i] =
  let code = T.concat [ varToGo var
                      , " = unsafe.Pointer(big.NewInt(*(*int64)("
                      , lVarToGo i
                      , ")))"
                      ]
  in Line (Just var) [ lVarToVar i ] code
primToGo var (LIntStr ITBig) [i] =
  let code = T.concat [ varToGo var
                      , " = MkString((*big.Int)("
                      , lVarToGo i
                      , ").String())"
                      ]
  in Line (Just var) [ lVarToVar i ] code
primToGo var (LIntStr ITNative) [i] =
  let code = T.concat [ varToGo var
                      , " = MkString(strconv.FormatInt(*(*int64)("
                      , lVarToGo i
                      , "), 10))"
                      ]
  in Line (Just var) [ lVarToVar i ] code
primToGo var LStrEq [left, right] =
  let code = T.concat [ varToGo var
                      , " = MkIntFromBool(*(*string)("
                      , lVarToGo left
                      , ") == *(*string)("
                      , lVarToGo right
                      , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var LStrCons [c, s] =
   let code = T.concat [ varToGo var
                      , " = MkString(string(*(*rune)("
                      , lVarToGo c
                      , ")) + *(*string)("
                      , lVarToGo s
                      , "))"
                      ]
  in Line (Just var) [ lVarToVar c, lVarToVar s ] code
primToGo var LStrHead [s] =
  let code = T.concat [ varToGo var
                      , " = MkRune(RuneAtIndex(*(*string)("
                      , lVarToGo s
                      , "), 0))"
                      ]
  in Line (Just var) [ lVarToVar s ] code
primToGo var LStrTail [s] =
  let code = T.concat [ varToGo var
                      , " = MkString(StrTail(*(*string)("
                      , lVarToGo s
                      , ")))"
                      ]
  in Line (Just var) [ lVarToVar s ] code
primToGo var LStrConcat [left, right] =
   let code = T.concat [ varToGo var
                      , " = MkString(*(*string)("
                      , lVarToGo left
                      , ") + *(*string)("
                      , lVarToGo right
                      , "))"
                      ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code
primToGo var LWriteStr [world, s] =
  let code = T.concat [ varToGo var
                      , " = WriteStr("
                      , lVarToGo s
                      , ")"
                      ]
  in Line (Just var) [ lVarToVar world, lVarToVar s ] code
primToGo var (LTimes (ATInt ITNative)) [left, right] = nativeIntBinOp var left right "*"
primToGo var (LTimes (ATInt ITBig)) [left, right] = bigIntBigOp var left right "Mul"
primToGo _ fn _ = Line Nothing [] (sformat ("panic(\"Unimplemented PrimFn: " % string % "\")") (show fn))

bigIntBigOp :: Var -> LVar -> LVar -> T.Text -> Line
bigIntBigOp var left right op =
  let code = T.concat [ varToGo var
                       , " = unsafe.Pointer(new(big.Int)."
                       , op
                       , "((*big.Int)("
                       , lVarToGo left
                       , "), (*big.Int)("
                       , lVarToGo right
                       , ")))"
                       ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code

nativeIntBinOp :: Var -> LVar -> LVar -> T.Text -> Line
nativeIntBinOp var left right op =
  let code = T.concat [ varToGo var
                       , " = MkInt(*(*int64)("
                       , lVarToGo left
                       , ") "
                       , op
                       , " *(*int64)("
                       , lVarToGo right
                       , "))"
                       ]
  in Line (Just var) [ lVarToVar left, lVarToVar right ] code


data TailCall = Self
              | Other
  deriving (Eq, Show)

containsTailCall :: Name -> SExp -> [TailCall]
containsTailCall self (SApp True n _) = if self == n
  then [ Self ]
  else [ Other ]
containsTailCall self (SLet _ a b) = containsTailCall self a ++ containsTailCall self b
containsTailCall self (SUpdate _ e) = containsTailCall self e
containsTailCall self (SCase _ _ alts) = concatMap (altContainsTailCall self) alts
containsTailCall self (SChkCase _ alts) = concatMap (altContainsTailCall self) alts
containsTailCall _ _ = []

altContainsTailCall :: Name -> SAlt -> [TailCall]
altContainsTailCall self (SConCase _ _ _ _ e) = containsTailCall self e
altContainsTailCall self (SConstCase _ e)     = containsTailCall self e
altContainsTailCall self (SDefaultCase e)     = containsTailCall self e


extractUsedVars :: [Line] -> S.Set Var
extractUsedVars lines = S.fromList (concat [used | Line _ used _ <- lines])

filterUnusedLines :: [Line] -> [Line]
filterUnusedLines lines =
  let usedVars = extractUsedVars lines
      requiredLines = mapMaybe (required usedVars) lines
  in if length lines /= length requiredLines
     -- the filtered lines might have made some other lines obsolete, filter again
     then filterUnusedLines requiredLines
     else lines
  where
    required _ l@(Line Nothing _ _) = Just l
    required _ l@(Line (Just RVal) _ _) = Just l
    required usedVars l@(Line (Just v) _ _) =
      if S.member v usedVars
      then Just l
      else Nothing


funToGo :: (Name, SDecl, [TailCall]) -> CG T.Text
funToGo (name, SFun _ args locs expr, tailCalls) = do
  bodyLines <- fmap filterUnusedLines (exprToGo name RVal expr)
  let usedVars = extractUsedVars bodyLines
  pure . T.concat $
    [ "// "
    , T.pack $ show name
    ,  "\nfunc "
    , nameToGo name
    , "("
    , T.intercalate ", " [ sformat ("_" % int % " unsafe.Pointer") i | i <- [0..length args-1]]
    , ") "
    , if returnsThunk then "(thunk, unsafe.Pointer)" else "unsafe.Pointer"
    , " {\n    var __rval unsafe.Pointer\n"
    , if returnsThunk then "var __thunk thunk\n" else T.empty
    , reserve usedVars
    , tailCallEntry
    , T.unlines [ line | Line _ _ line <- bodyLines ]
    , if returnsThunk then "return __thunk, __rval" else "return __rval"
    , "\n}\n\n"
    ]
  where
    returnsThunk = Other `elem` tailCalls
    tailCallEntry = if Self `elem` tailCalls
      then "entry:"
      else T.empty
    loc usedVars i =
      let i' = length args + i in
      if S.member (V i') usedVars
      then Just $ sformat ("_" % int) i'
      else Nothing
    reserve usedVars = case mapMaybe (loc usedVars) [0..locs] of
      [] -> T.empty
      usedLocs -> "    var " `T.append` T.intercalate ", " usedLocs `T.append` " unsafe.Pointer\n"

genMain :: T.Text
genMain = "func main() { runMain0() }"

codegenGo :: CodeGenerator
codegenGo ci = do
  let funs = [ (name, fun, containsTailCall name expr)
             | (name, fun@(SFun _ _ _ expr)) <- simpleDecls ci
             ]
      needsTrampolineByName = M.fromList [ (name, Other `elem` tailCalls)
                                         | (name, _, tailCalls) <- funs
                                         ]
      trampolineLookup = fromMaybe False . (`M.lookup` needsTrampolineByName)
      funCodes = evalState (traverse funToGo funs) (createCgState trampolineLookup)
      code = T.concat [ goPreamble (map (T.pack . show) (includes ci))
                      , T.concat funCodes
                      , genMain
                      ]
  withFile (outputFile ci) WriteMode $ \hOut -> do
    (Just hIn, _, _, p) <-
      createProcess (proc "gofmt" []){ std_in = CreatePipe, std_out = UseHandle hOut }
    TIO.hPutStr hIn code
    _ <- waitForProcess p
    return ()
