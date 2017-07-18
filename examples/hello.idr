module Main

import Data.Vect

%default total

-- The FFI

data GoFn : Type -> Type where
  MkGoFn : (x : t) -> GoFn t
%used MkGoFn x

mutual
  data Go_FnTypes : Type -> Type where
    Go_Fn     : Go_Types s -> Go_FnTypes t -> Go_FnTypes (s -> t)
    Go_FnIO   : Go_Types t -> Go_FnTypes (GIO t)
    Go_FnBase : Go_Types t -> Go_FnTypes t

  data Go_Types : Type -> Type where
    Go_Str  : Go_Types String
    Go_Unit : Go_Types ()
    Go_Any  : Go_Types (FFI_C.Raw a)

    Go_FnT  : Go_FnTypes a -> Go_Types (GoFn a)

  FFI_Go : FFI
  FFI_Go = MkFFI Go_Types String String

  GIO : Type -> Type
  GIO = IO' FFI_Go

%inline
gocall : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_Go [] ty} -> ty
gocall fname ty = foreign FFI_Go fname ty

-- Test/example code

-- XXX why is that required
partial
go : GIO () -> GIO ()
go action = gocall "Go" (Raw (GIO ()) -> GIO ()) (MkRaw action)

out : String -> GIO ()
out s = gocall "print" (String -> GIO ()) s

fourInts : Vect 4 Int
fourInts = [428, 1, 2, 3]

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

forLoop : List a -> (a -> GIO ()) -> GIO ()
forLoop [] f = pure ()
forLoop (x :: xs) f = do
  f x
  forLoop xs f

syntax for {x} "in" [xs] ":" [body] = forLoop xs (\x => body)

partial
main : GIO ()
main = do
  go $ for x in [1..10]:
    putStrLn' $ "Hello from goroutine " ++ show x
  out "foo\n"
  print' (isEven 4)
  putStrLn' ""
  print' fourInts

  for x in [1..20]:
    putStrLn' $ "Hello from main " ++ show x
