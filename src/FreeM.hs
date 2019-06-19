{-# language DeriveFunctor
           , TemplateHaskell
           , FlexibleContexts #-}
module FreeM where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import qualified System.Exit as S

data Tty c = GetL (String -> c)
           | PutLn String c
           | ExitSuccess
           deriving Functor

makeFree ''Tty

type TtyM = Free Tty

runTtyIO :: TtyM a -> IO a
runTtyIO = iterM run where
  run (GetL f) = getLine >>= f
  run (PutLn s f) = putStrLn s >> f
  run (ExitSuccess) = S.exitSuccess

echo :: TtyM ()
echo = getL >>= putLn

main' :: IO ()
main' = runTtyIO echo
