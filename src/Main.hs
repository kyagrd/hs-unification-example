module Main where
import           Syntax
import           Unbound.LocallyNameless
import           Unif1
-- import           Unif2
import qualified Data.Map.Strict as M

x,y,z :: Nm
x = s2n "x"
y = s2n "y"
z = s2n "z"

emptyMap = M.empty

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "hello world"
