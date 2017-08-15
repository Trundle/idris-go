module Main

mutual
  -- N.B. isOdd call gets inlined and the resulting self-recursive
  -- tailcall will result in a goto
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

main : IO ()
main = do
  printLn (isOdd 1234567)
  printLn (sum [1..50000])
