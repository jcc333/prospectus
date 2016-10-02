{-# LANGUAGE LambdaCase, MultiWayIf, ExplicitForAll, ScopedTypeVars, RankNTypes, LiberalTypeSynonyms, BangPatterns #-}
module Lib
    ( someFunc
    ) where

import Control.Monad.Free
import Data.List
import Data.Map
import Numeric.Natural

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TermF id v a = Struct id [ TermF id v a ]
                | Var id
                | Val v
                deriving (Eq, Show)

type Term id v a = Free (TermF id v a)

data Result id v a = Fail
                   | Bind [(TermF id v a, TermF id v a)]
                   deriving (Eq, Show)

succeed :: (id, v, a) => Result id v a -> Bool
succeed Fail = False
succeed _    = True

class (id, v, a) => (EvalTerm id v a) u =
  eval :: Term id v a -> u id v a -> Result id v a

class (a, m) => (Gen a) g =
  gen :: m g a -> a

continue0 :: (id, a) => TermF id v a -> Term id v a


instance (Gen a) => (EvalTerm id v a) (TermF id v a) where
  eval (Var _) = Fail
  eval (Val v) = Bind []
  eval (Struct id args) =


unifyList :: [Term id v a] -> [Term id v a] -> Maybe Substitution
unifyList [] [] = Just true
unifyList [] _  = Fail
unifyList _ []  = Fail
unifyList (x:xs) (y:ys) = foldl  (unify
  do s <- unify x y
     s' <- unifyList (apply s xs) (apply s ys)
     return (s ++ s')

unify (Var id n) (Var id m) = Bind [(Var x n, Var y m)]
unify (Var id n) term       = Bind [(Var x n,       y)]
unify term       (Var id m) = Bind [(Var y m,       x)]
unify (Struct a xs) (Struct b ys)
      | a == b = unifyList xs ys
      | otherwise  = Fail


