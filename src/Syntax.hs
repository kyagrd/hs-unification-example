{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Syntax where
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

