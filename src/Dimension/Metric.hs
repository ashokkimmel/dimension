{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}
module Dimensions.Metric (Metric(..)) where
import Data.Type.Ord (Compare)
import GHC.TypeLits (Nat,Symbol)
import Data.Kind (Type)
import Dimensions.Polymorphic.Parser (ToDimension)
type Metric :: Type 
data Metric = Meter | Kilogram | Second | Ampere | Kelvin | Mole | Candela
    deriving stock (Show, Eq)
type ToMetricDimension :: Symbol -> Metric
type family ToMetricDimension a where 
    ToMetricDimension "m" = 'Meter
    ToMetricDimension "kg" = 'Kilogram
    ToMetricDimension "s" = 'Second
    ToMetricDimension "A" = 'Ampere
    ToMetricDimension "K" = 'Kelvin
    ToMetricDimension "mol" = 'Mole
    ToMetricDimension "cd" = 'Candela
    ToMetricDimension "meter" = 'Meter
    ToMetricDimension "kilogram" = 'Kilogram
    ToMetricDimension "second" = 'Second
    ToMetricDimension "ampere" = 'Ampere
    ToMetricDimension "kelvin" = 'Kelvin
    ToMetricDimension "mole" = 'Mole
    ToMetricDimension "candela" = 'Candela
type instance ToDimension a = ToMetricDimension a
type ToNat :: Metric -> Nat
type family ToNat a where
    ToNat 'Meter    = 0
    ToNat 'Kilogram = 1
    ToNat 'Second   = 2
    ToNat 'Ampere   = 3
    ToNat 'Kelvin   = 4
    ToNat 'Mole     = 5
    ToNat 'Candela  = 6
-- ToNat serves only the purpose of ordering Metrics
type instance Compare a b = Compare (ToNat a) (ToNat b)
