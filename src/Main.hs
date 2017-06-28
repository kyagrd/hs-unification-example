module Main where
import           Syntax
import           Unbound.LocallyNameless
import           Unif1
-- import           Unif2
import qualified Data.Map.Strict as M
import           Data.List (nub)


-- following are all aeq
aaa = permbind (nub $ fv rule1 :: [Nm]) rule1
bbb = permbind [x,y] rule1
ccc = permbind [y,x] rule1
ddd = rulebind rule1


rule1 = (dec[enc[V x,V y], V y],  V x)
rulebind rule = permbind (nub $ fv rule :: [Nm]) rule

dec = D"dec"
enc = D"enc"

x,y,z :: Nm
x = s2n "x"
y = s2n "y"
z = s2n "z"

emptyMap = M.empty

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "hello world"
