{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where
import qualified Data.Sequence        as S
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Function

infixr 5 :->
data Type = I | Type :-> Type deriving (Show, Eq)

data Term = IntLiteral Int | PlusLiteral
          | Var Int | App Term Term
          | Lam Type Term
          deriving (Show, Eq)

data Avail = Used | Unused deriving Show
type Env = S.Seq (Type, Avail)

newtype Typecheck a = Typecheck { runTypecheck :: StateT Env (Except String) a }
                    deriving ( Functor, Applicative, Monad
                             , MonadState Env
                             , MonadError String)
