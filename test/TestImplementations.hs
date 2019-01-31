module TestImplementations where

import Control.Eff
import Control.Eff.Extend

import Interfaces

data Hole = Hole

runTestEnv :: Lifted IO r => [String] -> Eff (Env ': r) a -> Eff r a
runTestEnv _ (Val x) = pure x
runTestEnv testArgs (E u q) =
  case decomp q of
    Right GetArgs -> pure testArgs >>= \args -> runTestEnv testArgs (qApp u args)
    Right (PrintString s) -> lift (putStrLn s) >> runTestEnv testArgs (qApp u ())
    Left u0 -> E ident u0 >>= runTestEnv testArgs . qApp u
