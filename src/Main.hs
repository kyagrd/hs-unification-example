module Main where
import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict         as M
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

rules = map rulebind [rule1]
rule1 = (dec[enc[V x,V y], V y],  V x)
rulebind rule = permbind (nub $ fv rule :: [Nm]) rule

choose [] = empty
choose xs = do (xs1,x:xs2) <- init $ zip (inits xs) (tails xs)
               return (x, xs1++xs2)

subterm t@(V _)    = return (id, t)
subterm t@(D f []) = return (id, t)
subterm t@(D f ts) = return (id, t) <|>
  do (ts1,u:ts2) <- init $ zip (inits ts) (tails ts)
     let stctx x = D f $ ts1++x:ts2
     (stctx', u') <- subterm u
     return (stctx . stctx', u')

-- non variable positions only
subtermInit t = do { r@(stctx, D _ _) <- subterm t; return r }


mytm' = D"_"[mytm,V x,V y]
mytm = dec[V x,V y]

-- implementation of narrowing from
-- https://pdfs.semanticscholar.org/1803/a29f9588026a731a1baf4cc61a0d328d3e06.pdf
narr (t,positions,ss) =
  do ((stctx, t), ps) <- choose positions
     rule <- rules
     runFreshMT $
       do (_,(l,r)) <- unbind rule
          s <- u ([l`Eq`t],emptyMap)
          let subs = expand s
          return (subs . stctx . subs $ r, ps, s:ss)


dec = D"dec"
enc = D"enc"


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
