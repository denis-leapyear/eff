module Implementations where

import Control.Eff
import Control.Eff.Extend
import Data.Function (fix)
import System.Environment as System
import System.IO as SystemIO

import Interfaces

runEnv :: Lifted IO r => Eff (Env ': r) a -> Eff r a
runEnv (Val x) = pure x
runEnv (E u q) =
  case decomp q of
    Right GetArgs -> lift (System.getArgs) >>= \args -> runEnv (qApp u args)
    Right (PrintString s) -> lift (putStrLn s) >> runEnv (qApp u ())
    Left u0 -> E ident u0 >>= runEnv . qApp u

runFileReader :: Lifted IO r => Eff (FileReader ': r) a -> Eff r a
runFileReader (Val x) = pure x
runFileReader (E u q) =
  case decomp q of
    Right (OpenFile path) ->
      lift (do
        systemHandle <- SystemIO.openFile path ReadMode
        pure $ FileHandle { fileHandle = systemHandle }
      ) >>= \args -> runFileReader (qApp u args)
    Right (ReadFileContent FileHandle{..}) ->
      lift (SystemIO.hGetContents fileHandle) >>=
        \args -> runFileReader (qApp u args)
    Left u0 -> E ident u0 >>= runFileReader . qApp u
