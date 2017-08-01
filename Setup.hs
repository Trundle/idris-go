import qualified Distribution.Simple.Program     as P
import qualified Distribution.Simple.Setup       as S

import           Distribution.Simple

make verbosity = P.runProgramInvocation verbosity . P.simpleProgramInvocation "make"
buildGoLibs _ flags _ _ = do
  let verbosity = S.fromFlag $ S.buildVerbosity flags
  putStrLn "Building libraries..."
  make verbosity ["-C", "libs", "build" ]
installGoLibs verbosity = do
  putStrLn "Installing libraries..."
  make verbosity ["-C", "libs", "install" ]

main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = buildGoLibs
  , postCopy = \_ flags pkg local ->
                 installGoLibs (S.fromFlag $ S.copyVerbosity flags)
  , postInst = \_ flags pkg local ->
                  installGoLibs (S.fromFlag $ S.installVerbosity flags)
  }
