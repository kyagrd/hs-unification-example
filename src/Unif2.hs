module Unif2 where
import           Control.Applicative
import           Data.List               hiding (insert, map, null)
import           Data.Map.Strict         hiding (insert, map, mapMaybe, null)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Syntax
import           Unbound.LocallyNameless
{-# ANN module "HLint: ignore Use fmap" #-}
{-# ANN module "HLint: ignore Use mappend" #-}

{-
Alternative implemntation of the rule-based unfication algorithm U
from the unfication chapter of the "handbook of automated reasoning"
http://www.cs.bu.edu/~snyder/publications/UnifChapter.pdf

Instead of applying to the unification to the rest of the equation es,
make a feedback when there already exists a mapping from same variable
and do a deep occurs check following the links of the variables appearing
in the range term, which may be in the domain of the substitution so that
they require further following the links. There is also a helper function
expand that expands the term according to the current substitution.
-}

deepOccurs s x = occurs x . expand s

expand s (V x) = case M.lookup x s of { Nothing -> V x; Just u -> expand s u }
expand s (D "f" ts) = D "f" (expand s <$> ts)

ustep :: Monad m => ([Eqn], Map Nm Tm) -> m ([Eqn], Map Nm Tm)
ustep (t1@(D f1 ts1) `Eq` t2@(D f2 ts2) : es, s)
  | f1==f2 && length ts1==length ts2 = return (zipWith Eq ts1 ts2 ++ es, s)
  | otherwise = fail $ show t1 ++" /= " ++ show t2
                    ++ let t1' = expand s t1; t2' = expand s t2 in
                         if t1 /= t1' || t2 /= t2'
                           then ", that is, "++show t1 ++" /= " ++ show t2
                           else ""
ustep (t1@(D _ _) `Eq` t2@(V x) : es, s) = return (t2 `Eq` t1 : es, s)
ustep (t1@(V x) `Eq` t2@(V y) : es, s)
  | x == y = return (es, s)
  | x > y = ustep (t2 `Eq` t1 : es, s)
ustep (V x `Eq` t : es, s)
  | deepOccurs s x t = fail $ show x ++" occurs in "++show t
                           ++ let t' = expand s t in
                                if t /= t' then ", that is, "++show t' else ""
  | M.member x s = return (s!x `Eq` t : es, s')
  | otherwise = return (es, s')
    where
      s' = M.insert x t s

u :: Monad m => ([Eqn], Map Nm Tm) -> m (Map Nm Tm)
u ([], s) = return s
u (es, s) = u =<< ustep (es, s)
