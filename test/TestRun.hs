module Main where

import           Control.Monad        (filterM, when)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BSU
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      (FilePath (..), takeBaseName, (</>))
import           System.IO            (hPutStrLn, stderr)
import           System.Process       (CreateProcess (..), proc,
                                       readCreateProcessWithExitCode)
import           Test.Tasty           (TestTree (..), defaultMain, testGroup)
import qualified Test.Tasty.Golden    as G

testDirectory :: FilePath
testDirectory = "test"

listTests :: IO [FilePath]
listTests = do
  contents <- map (testDirectory </>) <$> listDirectory testDirectory
  filterM doesDirectoryExist contents

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName = takeBaseName path
      expected = path </> "expected"
  return $ G.goldenVsString testName expected action
  where
    action :: IO BS.ByteString
    action = do
      let testRun = (proc "./run" []) { cwd = Just path }
      (_, output, errorOut) <- readCreateProcessWithExitCode testRun ""
      when (errorOut /= "") $ hPutStrLn stderr ("\nError: " ++ path ++ "\n" ++ errorOut)
      return $ BSU.fromString output

main :: IO ()
main = do
  paths <- listTests
  goldens <- mapM mkGoldenTest paths
  defaultMain (testGroup "End to end tests" goldens)
