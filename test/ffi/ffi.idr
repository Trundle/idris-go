module Main

import Go

%include Go "strings"
upper : String -> String
upper s = unsafePerformIO $
  gocall (Function "strings.ToUpper") (String -> GIO String) s

trim : String -> String -> String
trim s cutset = unsafePerformIO $
  gocall (Function "strings.Trim") (String -> String -> GIO String) s cutset


main : GIO ()
main = do
  putStrLn' $ upper "foo"
  putStrLn' $ trim "foobar" "fr"
