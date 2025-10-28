{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe #-}
module Dimensions.Data (Dimension(..),liftD2,(!<*>)) where 
import GHC.TypeLits (KnownSymbol)
import Data.Kind (Type)
import Dimensions.Printer (Print)
import Dimensions.GetTermLevel qualified as TT 
import Dimensions.TypeLevelInt (Int')
type role Dimension nominal representational
type Dimension :: forall k. [(k,Int')] -> Type -> Type 
newtype Dimension a b = MkDimension b
    deriving stock (Eq,Ord,Functor)

--deriving newtype instance (Num b,ValidDimension a) => Num (Dimension a b) 
--deriving newtype instance (Fractional b,ValidDimension a) => Fractional (Dimension a b) 
instance (Show b,KnownSymbol (Print dim)) => Show (Dimension dim b) where
    show (MkDimension a) =  show a ++ ' ' : TT.symbolVal (Print dim)

liftD2 :: (a -> b -> c) -> Dimension x a -> Dimension x b -> Dimension x c
liftD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b) 

(!<*>) :: Dimension x (a -> b) -> Dimension x a -> Dimension x b 
(!<*>) (MkDimension a) (MkDimension b) = MkDimension (a b)


instance Num a => Num (Dimension '[] a) where 
    MkDimension a + MkDimension b = MkDimension (a + b)
    MkDimension a - MkDimension b = MkDimension (a - b)
    MkDimension a * MkDimension b = MkDimension (a * b)
    negate (MkDimension a) = MkDimension (negate a)
    abs (MkDimension a) = MkDimension (abs a)
    signum (MkDimension a) = MkDimension (signum a)
    fromInteger n = MkDimension (fromInteger n)

