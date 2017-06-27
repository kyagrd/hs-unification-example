{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Syntax where

import           Data.List               hiding (insert, map, null)
import           Data.Map.Strict         hiding (insert, map, null)
import qualified Data.Map.Strict         as M
import           Unbound.LocallyNameless

type Nm = Name Tm
type Sym = String

data Tm = V Nm | D Sym [Tm] deriving (Eq,Ord,Show)
data Eqn = Eq Tm Tm deriving Show

$(derive [''Tm, ''Eqn])

instance Alpha Tm
instance Alpha Eqn

instance Subst Tm Tm where
  isvar (V x) = Just (SubstName x)
  isvar _     = Nothing
instance Subst Tm Eqn

occurs x t = x `elem` (fv t :: [Nm])

{-
Straightforward implemntation of the rule-based unfication algorithm U
from the unfication chapter of the "handbook of automated reasoning"
http://www.cs.bu.edu/~snyder/publications/UnifChapter.pdf
-}

ustep :: Monad m => ([Eqn], Map Nm Tm) -> m ([Eqn], Map Nm Tm)
ustep (t1@(D f1 ts1) `Eq` t2@(D f2 ts2) : es, s)
  | f1==f2 && length ts1==length ts2 = return (zipWith Eq ts1 ts2 ++ es, s)
  | otherwise = fail $ show t1 ++" /= " ++ show t2
ustep (t1@(D _ _) `Eq` t2@(V x) : es, s) = return (t2 `Eq` t1 : es, s)
ustep (t1@(V x) `Eq` t2@(V y) : es, s)
  | x == y = return (es, s)
  | x > y = ustep (t2 `Eq` t1 : es, s)
ustep (V x `Eq` t : es, s)
  | occurs x t = fail $ show x ++" occurs in "++show t
  | otherwise = return (es', s')
    where
      es' = subst x t es
      s' = M.insert x t s

u :: Monad m => ([Eqn], Map Nm Tm) -> m (Map Nm Tm)
u ([], s) = return s
u (es, s) = u =<< ustep (es, s)
