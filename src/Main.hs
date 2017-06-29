module Main where
import           Control.Applicative
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict         as M
import           Data.Set                (Set (..))
import qualified Data.Set                as S
import           Syntax
import           Unbound.LocallyNameless hiding (empty)
-- import           Unif1
import           Unif2
{-# ANN module "HLint: ignore Use fmap" #-}
{-# ANN module "HLint: ignore Use mappend" #-}

-- following are all aeq
aaa = permbind (nub $ fv rule1 :: [Nm]) rule1
bbb = permbind [x,y] rule1
ccc = permbind [y,x] rule1
ddd = rulebind rule1


rulebind rule = permbind (nub $ fv rule :: [Nm]) rule

dec = D"dec"
enc = D"enc"
rule1 = (dec[enc[V x,V y], V y],  V x)
ruleSet1 = map rulebind [rule1]


mytmDec2 = D"_"[dec[V x,V y],dec[V z,V y]]
mytmDec1 = dec[V x,V y]
mytmDec1' = D"_"[mytmDec1,V x,V y]

ruleSet2 = map rulebind
  [ (D"h"[D"f"[V x],V y],  V y)
  , (D"h"[D"g"[V x],V y],  V y)
  ]

mytmH' = D"_"[mytmH,V x,V y]
mytmH = h[V x,V y] where h=D"h"

data Pos = PR | PD Int Pos deriving (Eq,Ord,Show)

plugPos _        PR         v = v
plugPos (D f ts) (PD k pos) v = D f $ ts1 ++ plugPos u pos v : ts2
  where (ts1,u:ts2) = splitAt k ts

subterm t@(V _)    = return (PR, t)
subterm t@(D f []) = return (PR, t)
subterm t@(D f ts) = return (PR, t) <|>
  do k <- [0 .. length ts - 1]
     (pos, v) <- subterm (ts!!k)
     return (PD k pos, v)

-- non variable positions only
subtermInit t = do { r@(pos, D _ _) <- subterm t; return pos }

atPos PR         t        = t
atPos (PD k pos) (D _ ts) = atPos pos (ts!!k)

-- implementation of narrowing from
-- https://pdfs.semanticscholar.org/1803/a29f9588026a731a1baf4cc61a0d328d3e06.pdf
narrBy rules (t,poss,s) =
  do pos <- poss
     rule <- rules
     runFreshMT $
       do mapM_ fresh (nub $ fv t :: [Nm])
          (_,(l,r)) <- unbind rule
          s1 <- u ([l `Eq` atPos pos t],emptyMap)
          let s2 = foldr extend s (M.toList s1)
          let subs = expand s1
          return (plugPos (subs t) pos (subs r), poss\\[pos], s2)
     where
       extend (x,t) s = let t' = expand s t
                         in M.insert x t' (subst x t' <$> s)

kleeneClosure step x = return x <|> transClosure step x
transClosure step x = xs' <|> asum (transClosure step <$> xs')
  where xs' = step x

subvariant rules t =
  (\(a,_,c)->(a,c)) <$> kleeneClosure (narrBy rules) (t, subtermInit t, emptyMap)


-- example unification (finding unifier modulo subterm convergnet rewriting)
-- basically implmented http://www.lsv.fr/~ciobaca/subvariant/

unifiersModulo rules (t1,t2) =
  do (D"_"[t1',t2'], _, s) <- kleeneClosure (narrBy rules)
                                (D"_"[t1,t2], subtermInit $ D"_"[t1,t2], emptyMap)
     s <- u ([t1' `Eq ` t2'], s)
     return $ M.filterWithKey (\k _ -> k `elem` fvs) s
  where
    fvs = nub $ fv (t1,t2) :: [Nm]

x,y,z :: Nm
x = s2n "x"
y = s2n "y"
z = s2n "z"

emptyMap = M.empty

es1 = [ D"f"[V x,D"f"[V y]] `Eq` D"f"[V z,V x] ]
es2 = [ V x `Eq` D"f"[V y], V y `Eq` D"f"[V z], V z `Eq` D"f"[V x] ]

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "hello world"
