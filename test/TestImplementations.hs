module TestImplementations where

import Control.Eff
import Control.Eff.Extend

import Interfaces

runTestEnv :: Lifted IO r => [String] -> Eff (Env ': r) a -> Eff r a
runTestEnv _ (Val x) = pure x
runTestEnv testArgs (E arrows union) =
  case decomp union of
    Right GetArgs -> pure testArgs >>= \args -> runTestEnv testArgs (qApp arrows args)
    Right (PrintString s) -> lift (putStrLn s) >> runTestEnv testArgs (qApp arrows ())
    Left u0 -> E ident u0 >>= runTestEnv testArgs . qApp arrows
