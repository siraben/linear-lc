{-# LANGUAGE OverloadedLists #-}
module Typecheck where
import Types
import qualified Data.Sequence        as S
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Function

typeof :: Term -> Typecheck Type
typeof (IntLiteral _) = pure I
typeof PlusLiteral = pure (I :-> I)
typeof (Var n) = do
  e <- get
  case e S.!? n of
    Nothing -> throwError $ "unbound var " <> show n
    Just (_, Used) -> throwError $ "var " <> show n <> " has already been used"
    Just (t, Unused) -> modify (S.adjust use n) *> pure t
      where use (v, Unused) = (v, Used)
typeof (App a b) = do
  tf <- typeof a
  tb <- typeof b
  case tf of
    tx :-> ty | tx == tb -> pure ty
    _                    -> throwError "failed to apply function"
typeof e@(Lam a b) = do
  tb <- modify ([(a, Unused)] S.><) *> typeof b
  h <- (S.!? 0) <$> get
  case h of
    Just (_, Unused) -> throwError $ "var 0 was not used in " <> show e
    Just (_, Used)   -> modify (S.drop 1) *> pure (a :-> tb)
    Nothing          -> error "impossible"

typecheck e = typeof e
            & runTypecheck
            & (`evalStateT` mempty)
            & runExcept

-- Succeeds
-- \x -> (\x -> x) x
ex1 = Lam I (App (Lam I (Var 0)) (Var 0))

-- Fails
-- \x -> x + x
ex2 = Lam I (App (App PlusLiteral (Var 0)) (Var 0))

-- Fails
-- \x -> 1
ex3 = Lam I (IntLiteral 1)
