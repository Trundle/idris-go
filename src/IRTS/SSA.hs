module IRTS.SSA (toSSA) where

import           Control.Monad                    (forM, forM_)
import           Control.Monad.Trans.State.Strict (State, gets, modify,
                                                   runState)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (mapMaybe)
import           IRTS.Lang                        (LVar (..))
import           IRTS.Simplified

type BlockId = Int
newtype Block = Block { blockPreds  :: [BlockId]
                      }
  deriving (Show)

newBlock :: [BlockId] -> Block
newBlock preds = Block { blockPreds  = preds
                       }

data SSAConvState = SSAConvState { convStateNLocals      :: Int
                                 , convStateNextBlock    :: Int
                                 , convStateCurrentBlock :: Int
                                 , convStateBlocks       :: M.Map BlockId Block
                                 -- Var to (Block ID to Value)
                                 , convStateAssignments  :: M.Map Int (M.Map BlockId Int)
                                 , convStateNextPhi      :: Int
                                 , convStatePhis         :: M.Map Int Int
                                 }

type ConvOp a = State SSAConvState a

newConvState :: Int -> Int -> SSAConvState
newConvState numberOfArgs locals =
  let args = [ 0..numberOfArgs-1 ]
      initialAssignments = M.fromList [ (v, M.singleton 0 v) | v <- args ]
  in SSAConvState { convStateNLocals      = locals + numberOfArgs
                  , convStateNextBlock    = 0
                  , convStateCurrentBlock = 0
                  , convStateBlocks       = M.empty
                  , convStateAssignments  = initialAssignments
                  , convStateNextPhi      = 0
                  , convStatePhis         = M.empty
                  }

-- Go's escape analysis seems to perform better if variables are only assigned
-- once, hence do exactly that
toSSA :: SDecl -> (Int, SExp)
toSSA (SFun _ args locals expr) =
  let numberOfArgs = length args
      (expr', state) = runState (ssa expr) (newConvState numberOfArgs locals)
      phis = convStatePhis state
      argPhis = mapMaybe (\a -> (\v -> (a, v)) <$> M.lookup a phis) [0..numberOfArgs-1]
  in (convStateNLocals state, addLetsForArgs argPhis (fixPhis phis expr'))

addLetsForArgs :: [(Int, Int)] -> SExp -> SExp
addLetsForArgs argPhis expr = foldr addLet expr argPhis
  where
    addLet (a, v) = SLet (Loc v) (SV (Loc a))

fixPhis :: M.Map Int Int -> SExp -> SExp
fixPhis phis (SLet (Loc l) b expr) =
  let b' = fixPhis phis b
      expr' = fixPhis phis expr
  in case M.lookup l phis of
    Just l' -> SLet (Loc l') b' expr'
    Nothing -> SLet (Loc l) b' expr'
fixPhis phis (SCase caseType v alts) = fixPhisCase phis alts (SCase caseType v)
fixPhis _ expr = expr

fixPhisCase :: M.Map Int Int -> [SAlt] -> ([SAlt] -> SExp) -> SExp
fixPhisCase phis alts ctor =
  let alts' = map (fixPhisAlt phis) alts
  in ctor alts'

fixPhisAlt :: M.Map Int Int -> SAlt -> SAlt
fixPhisAlt phis (SConCase base tag name args expr) =
  let expr' = fixPhis phis expr
  in SConCase base tag name args expr'
fixPhisAlt phis (SConstCase c expr) =
  let expr' = fixPhis phis expr
  in SConstCase c expr'
fixPhisAlt phis (SDefaultCase expr) =
  let expr' = fixPhis phis expr
  in SDefaultCase expr'

ssa :: SExp -> ConvOp SExp
ssa (SV r) = do
  r' <- readVar r
  return (SV r')
ssa (SApp tc n args) = do
  args' <- traverse readVar args
  return (SApp tc n args')
ssa (SLet l b expr) = do
  b' <- ssa b
  l' <- assign l
  expr' <- ssa expr
  return (SLet l' b' expr')
ssa (SCon realloc tag name args) = do
  args' <- traverse readVar args
  return (SCon realloc tag name args')
ssa (SCase caseType l alts) = ssaCase l alts (SCase caseType)
ssa (SChkCase v alts) = ssaCase v alts SChkCase
ssa (SOp fn args) = do
  args' <- traverse readVar args
  return (SOp fn args')
ssa (SForeign ty f args) = do
  args' <- forM args $ \(fdesc, var) -> do
    var' <- readVar var
    return (fdesc, var')
  return (SForeign ty f args')
ssa expr = return expr

ssaCase :: LVar -> [SAlt] -> (LVar -> [SAlt] -> SExp) -> ConvOp SExp
ssaCase var alts ctor = do
  pred <- gets convStateCurrentBlock
  var' <- readVar var
  blocksAndAlts <- forM alts $ \alt -> do
    alt' <- ssaAlt pred alt
    blockId <- gets convStateCurrentBlock
    return (blockId, alt')
  enterNextBlock (map fst blocksAndAlts)
  return $ ctor var' (map snd blocksAndAlts)

ssaAlt :: BlockId -> SAlt -> ConvOp SAlt
ssaAlt pred (SConCase base tag name args expr) = do
  enterNextBlock [pred]
  let locals = map Loc [base .. base + length args - 1]
  locals' <- traverse assign locals
  let base' = if null locals'
        then base -- doesn't really matter, as it won't be used
        else (\(Loc i) -> i) (head locals')
  expr' <- ssa expr
  return (SConCase base' tag name args expr')
ssaAlt pred (SConstCase c expr) = ssaAltOnlyExpr pred expr (SConstCase c)
ssaAlt pred (SDefaultCase expr) = ssaAltOnlyExpr pred expr SDefaultCase

ssaAltOnlyExpr :: BlockId -> SExp -> (SExp -> SAlt) -> ConvOp SAlt
ssaAltOnlyExpr pred expr ctor = do
  enterNextBlock [pred]
  expr' <- ssa expr
  return (ctor expr')

nextLocal :: ConvOp Int
nextLocal = do
  i <- gets convStateNLocals
  modify (\s -> s { convStateNLocals = i + 1 })
  return $ i + 1

enterNextBlock :: [BlockId] -> ConvOp ()
enterNextBlock preds =
  modify (\s@SSAConvState{ convStateCurrentBlock = block
                         , convStateBlocks = blocks
                         } ->
            s { convStateCurrentBlock = block + 1
              , convStateBlocks = M.insert (block + 1) (newBlock preds) blocks
              })

assign :: LVar -> ConvOp LVar
assign var = do
  value <- nextLocal
  blockId <- gets convStateCurrentBlock
  writeVar var blockId (Loc value)

writeVar :: LVar -> BlockId -> LVar -> ConvOp LVar
writeVar (Loc var) blockId (Loc value) = do
  modify (\s@SSAConvState{ convStateAssignments = as } ->
             s { convStateAssignments = M.alter (update blockId) var as })
  return (Loc value)
  where
    update block Nothing  = Just (M.singleton block value)
    update block (Just m) = Just (M.insert block value m)
writeVar (Glob name) _ _ = error $ "Trying to assign global " ++ show name

readVar :: LVar -> ConvOp LVar
readVar var = do
  block <- gets convStateCurrentBlock
  readVarFromBlock var block

readVarFromBlock :: LVar -> BlockId -> ConvOp LVar
readVarFromBlock (Loc var) blockId = do
  valueByBlock <- M.lookup var <$> gets convStateAssignments
  case valueByBlock of
    Nothing -> error $ "Expected var " ++ show var ++ " not present"
    Just m -> case M.lookup blockId m of
      Nothing    -> readVarRecursive (Loc var) blockId
      Just value -> return (Loc value)
readVarFromBlock (Glob name) _ = error $ "Trying to read global " ++ show name

readVarRecursive :: LVar -> BlockId -> ConvOp LVar
readVarRecursive var blockId = do
  preds <- getPreds blockId
  if length preds == 1
    then readVarFromBlock var (head preds)
    else (do
             phi <- nextPhi
             value <- nextLocal
             _ <- writeVar var blockId (Loc value)
             addPhiOperands var value preds
             return (Loc value))

addPhiOperands :: LVar -> Int -> [BlockId] -> ConvOp ()
addPhiOperands var phi preds =
  forM_ preds $ \pred -> do
    Loc value <- readVarFromBlock var pred
    modify (\s@SSAConvState{ convStatePhis = phis } ->
              s { convStatePhis = M.insert value phi phis })

getPreds :: BlockId -> ConvOp [BlockId]
getPreds blockId = do
  block <- expectBlock blockId
  return (blockPreds block)

expectBlock :: BlockId -> ConvOp Block
expectBlock blockId = do
  maybeBlock <- M.lookup blockId <$> gets convStateBlocks
  case maybeBlock of
    Just block -> return block
    Nothing    -> error $ "Block " ++ show blockId ++ " doesn't exist"

nextPhi :: ConvOp Int
nextPhi = do
  phi <- gets convStateNextPhi
  modify (\s -> s { convStateNextPhi = phi - 1 })
  return $ phi - 1
