{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Syntax where
import           Control.Applicative
import           Unbound.LocallyNameless
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

type Nm = Name Tm
type Sym = String

data Eqn = Eq Tm Tm deriving Show
data Quan = All Nm | Ex Nm deriving (Eq,Ord,Show)
data Tm = V Nm | D Sym [Tm] deriving (Eq,Ord,Show)
data Form = A Sym [Tm] | Conj [Form] | Disj [Form]
           | Forall (Bind Nm Form) | Exists (Bind Nm Form) | Imp Form Form
           deriving (Eq,Ord,Show)

instance Eq (Bind Nm Form) where (==) = aeqBinders
instance Ord (Bind Nm Form) where compare = acompare

$(derive [''Tm, ''Eqn, ''Form, ''Quan])

instance Alpha Tm
instance Alpha Quan
instance Alpha Eqn
instance Alpha Form

instance Subst Tm Tm where
  isvar (V x) = Just (SubstName x)
  isvar _     = Nothing
instance Subst Tm Eqn
instance Subst Tm Quan
instance Subst Tm Form

occurs x t = x `elem` (fv t :: [Nm])

-- crude well-formedness check (no check for proper vairalbe bindings)
formA, formD, formG :: Form -> Bool

formA (A _ _) = True
formA _       = False

formD (A _ _)     = True
formD (Conj fs)   = all formD fs
formD (Forall b)  = formD . snd . unsafeUnbind $ b
formD (Imp fG fA) = formA fA && formG fG -- note: no check for "rigidly" atomic
formD _           = False

formG (A _ _)     = True
formG (Conj fs)   = all formG fs
formG (Disj fs)   = all formG fs
formG (Forall b)  = formG . snd . unsafeUnbind $ b
formG (Exists b)  = formG . snd . unsafeUnbind $ b
formG (Imp fD fG) = formD fD && formG fG

wfA, wfD, wfG :: Fresh m => [Quan] -> Form -> m Bool
wfA ctx f@(A _ _) = return $ all (`elem` (fv ctx :: [Nm])) (fv f :: [Nm])
wfA ctx _         = return False

wfD ctx f@(A _ _)   = wfA ctx f
wfD ctx (Conj fs)   = and <$> sequence (wfD ctx <$> fs)
wfD ctx (Forall b)  = wfD ctx <$> snd =<< unbind b
wfD ctx (Imp fG fA) = (&&) <$> wfG (flipQuan <$> ctx) fG <*> wfA ctx fA
wfD ctx _           = return False

wfG ctx f@(A _ _)   = return $ all (`elem` (fv ctx :: [Nm])) (fv f :: [Nm])
wfG ctx (Conj fs)   = and <$> sequence (wfD ctx <$> fs)
wfG ctx (Disj fs)   = and <$> sequence (wfD ctx <$> fs)
wfG ctx (Forall b)  = do { (x,fG) <- unbind b; wfG (All x:ctx) fG }
wfG ctx (Exists b)  = do { (x,fG) <- unbind b; wfG (Ex  x:ctx) fG }
wfG ctx (Imp fD fG) = (&&) <$> wfD (flipQuan <$> ctx) fD <*> wfG ctx fG

flipQuan (All x) = Ex x
flipQuan (Ex x)  = All x
