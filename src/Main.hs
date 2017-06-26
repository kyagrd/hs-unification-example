module Main where

import           Syntax
import           Unbound.LocallyNameless

x,y,z :: Nm
x = s2n "x"
y = s2n "y"
z = s2n "z"

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "hello world"
