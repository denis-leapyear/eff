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

runTestFileReader
  :: (Lifted IO r)
  => String -> Eff (FileReader String ': r) a -> Eff r a
runTestFileReader _ (Val x) = pure x
runTestFileReader content (E arrows union) =
  case decomp union of
    Right (OpenFile path) ->
      (pure path) >>= \args -> runTestFileReader content (qApp arrows args)
    Right (ReadFileContent _) ->
      (pure content) >>= \args -> runTestFileReader content (qApp arrows args)
    Left u0 -> E ident u0 >>= runTestFileReader content . qApp arrows