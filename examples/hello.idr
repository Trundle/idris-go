module Main

import Data.Vect
import Go

%default total

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
