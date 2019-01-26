module Types where

import Env (Env)

import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)


newtype Pm a = ReaderT (IORef Env) IO a
