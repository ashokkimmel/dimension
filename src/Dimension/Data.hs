{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes,DerivingStrategies #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe #-}
module Dimensions.Data (Dimension(..),liftD2,mapD) where 
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
mapD :: (a -> b) -> Dimension x a -> Dimension x b
mapD f (MkDimension a) = MkDimension (f a)
liftD2 :: (a -> b -> c) -> Dimension x a -> Dimension x b -> Dimension x c
liftD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b) 
