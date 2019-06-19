{-# language GeneralizedNewtypeDeriving #-}
module MonadStack where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import GHC.Generics

newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
  deriving (Functor, Monad, MonadState Int, MonadWriter [Int], MonadIO)

foo :: Stack ()
foo = do
  put 1             -- State layer
  tell [2]          -- Writer layer
  liftIO $ print 3  -- IO Layer
  return ()

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unStack m) 0)
