module Main

import Data.Vect
import Go

%default total

-- XXX why is that required
partial
go : GIO () -> GIO ()
go action = gocall (Function "Go") (Raw (GIO ()) -> GIO ()) (MkRaw action)

-- Some easy string functions to test the FFI

out : String -> GIO ()
out s = gocall (Function "print") (String -> GIO ()) s

-- %include will be translated to imports
%include Go "strings"
upper : String -> String
upper s = unsafePerformIO $
  gocall (Function "strings.ToUpper") (String -> GIO String) s

trim : String -> String -> String
trim s cutset = unsafePerformIO $
  gocall (Function "strings.Trim") (String -> String -> GIO String) s cutset


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
