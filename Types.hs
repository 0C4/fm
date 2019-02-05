{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, DeriveAnyClass #-}

module Types where

import Core
import Utils


import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State (StateT,MonadState)
import Data.IORef (IORef, readIORef)
import Data.Typeable (Typeable)
import System.Time


data Conf = Conf { un :: () }
newtype Fm a = Fm (ReaderT Conf (StateT Env IO) a)
             deriving (Functor, Monad, MonadFail, MonadIO,
                       MonadState Env, MonadReader Conf, Typeable)
