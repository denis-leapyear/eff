module Implementations where

import Control.Eff
import Control.Eff.Extend
import Data.Function (fix)
import System.Environment as System
import System.IO as SystemIO

import Interfaces

runEnv :: Lifted IO r => Eff (Env ': r) a -> Eff r a
runEnv (Val x) = pure x
runEnv (E arrows union) =
  case decomp union of
    Right GetArgs -> lift (System.getArgs) >>= \args -> runEnv (qApp arrows args)
    Right (PrintString s) -> lift (putStrLn s) >> runEnv (qApp arrows ())
    Left u0 -> E ident u0 >>= runEnv . qApp arrows

runFileReader
  :: (Lifted IO r)
  => Eff (FileReader SystemIO.Handle ': r) a -> Eff r a
runFileReader (Val x) = pure x
runFileReader (E arrows union) =
  case decomp union of
    Right (OpenFile path) ->
      lift (SystemIO.openFile path ReadMode) >>=
        \args -> runFileReader (qApp arrows args)
    Right (ReadFileContent systemHandle) ->
      lift (SystemIO.hGetContents systemHandle) >>=
        \args -> runFileReader (qApp arrows args)
    Left u0 -> E ident u0 >>= runFileReader . qApp arrows
