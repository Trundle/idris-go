module Main

import Data.Vect

%default total

-- The FFI

data GoInterface : String -> Type where
  MkInterface : (iface : String) -> GoInterface iface

data Go_FFI_Call = Function String
  | Method (GoInterface iface) String

mutual

  ||| Go foreign types
  data Go_Types : Type -> Type where
    Go_Str       : Go_Types String
    Go_Unit      : Go_Types ()
    Go_Interface : Go_Types (GoInterface a)
    Go_Nilable   : Go_Types a -> Go_Types (Maybe a)
    Go_Any       : Go_Types (FFI_C.Raw a)
    -- Note that this is actually only valid as return value
    Go_MultiVal  : (Go_Types a, Go_Types b) -> Go_Types (a, b)

  FFI_Go : FFI
  FFI_Go = MkFFI Go_Types Go_FFI_Call String

  GIO : Type -> Type
  GIO = IO' FFI_Go

%inline
gocall : (f : Go_FFI_Call) -> (ty : Type) -> {auto fty : FTy FFI_Go [] ty} -> ty
gocall f ty = foreign FFI_Go f ty


-- Test/example code

-- %include will be translated to an import
%include Go "net"

-- XXX why is that required
partial
go : GIO () -> GIO ()
go action = gocall (Function "Go") (Raw (GIO ()) -> GIO ()) (MkRaw action)

-- Some easy string functions to test the FFI

out : String -> GIO ()
out s = gocall (Function "print") (String -> GIO ()) s

%include Go "strings"
upper : String -> String
upper s = unsafePerformIO $
  gocall (Function "strings.ToUpper") (String -> GIO String) s

trim : String -> String -> String
trim s cutset = unsafePerformIO $
  gocall (Function "strings.Trim") (String -> String -> GIO String) s cutset


-- Networking!

Listener : Type
Listener = GoInterface "net.Listener"

Conn : Type
Conn = GoInterface "net.Conn"

GoError : Type
GoError = GoInterface "error"

toEither : (a, Maybe GoError) -> Either GoError a
toEither (_, Just e) = Left e
toEither (x, Nothing) = Right x

accept : Listener -> GIO (Either GoError Conn)
accept listener = do
  map toEither $ gocall (Method listener "Accept") (Listener -> GIO (Conn, Maybe GoError)) listener

listen : String -> String -> GIO (Either GoError Listener)
listen net laddr = do
   map toEither $ gocall (Function "net.Listen")
           (String -> String -> GIO (Listener, Maybe GoError))
           net laddr


-- Some other Idris niceties
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

  putStrLn' $ upper "foo"
  putStrLn' $ trim "foobar" "fr"

  Right listener <- listen "tcp" ":1234"
    | Left _ => putStrLn' "Could not create listener"
  Right conn <- accept listener
    | Left _ => putStrLn' "Accepting went wrong :("
  putStrLn' "Got a client!"
