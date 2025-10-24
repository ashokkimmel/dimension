{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE Safe #-}
module Dimensions.Ordered.Units (Dimension(..)
                    ,(!*),(!/),(!-),(!+)
                    ,Replace,Isos,Delete
                    ,Format,ValidDimension,ValidParse
                    ,mkisos,applypos,apply,same,transformpos,transform
                    ,validateDimension
                    ,undimension
                    ,dimensions
                    ,dimension
                    ,dimensionsPoly
                    ,dimensionPoly,divD,combine2D,lift2D
                    ,noParseDimensions,noParseDimension) where 
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol)
import qualified Dimensions.TypeLevelInt as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.Polymorphic.Parser (Parse)
import Data.Semigroup (Endo(appEndo,Endo),stimes)
import Dimensions.Ordered.Order (Sort,Merge)
import Data.Kind (Constraint)
import Dimensions.DimensionalMisc (Isos',Delete,UnZero,Replace',LookupD0,Invert)
import Dimensions.Data (Dimension(MkDimension),liftD2)
import qualified Dimensions.GetTermLevel as TT

type Replace :: k -> k -> [(k, Int')] -> [(k, Int')]
type Replace s t x = Sort (Replace' s t x)
type Isos :: [(a, a)] -> [(a, k)] -> [(k, Int')]
type Isos a b = Sort (Isos' a b)
type Format :: [(k, Int')] -> [(k, Int')]
type Format a = Sort (UnZero a)
type ValidDimension :: [(k, Int')] -> Constraint
type ValidDimension a = (a ~ Format a)
type ValidParse :: forall k. Symbol -> [(k,Int')]
type ValidParse a = Sort (Parse a)



(!+) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!+) = liftD2 (+)
infixl 6 !+
(!-) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!-) = liftD2 (-)
infixl 6 !-
dimension :: forall a -> forall b. b -> Dimension (ValidParse @Symbol a)  b
dimension _ = MkDimension 
dimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse @Symbol a) b)
dimensions _ = fmap MkDimension 
dimensionPoly :: forall a -> forall b.  b -> Dimension (ValidParse a) b 
dimensionPoly _ = MkDimension 
dimensionsPoly :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse a) b) 
dimensionsPoly _ = fmap MkDimension
noParseDimension :: forall a -> forall b. b -> Dimension (Format a) b
noParseDimension _ = MkDimension
noParseDimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (Format a) b)
noParseDimensions _ = fmap MkDimension

validateDimension :: Dimension a b -> Dimension (Format a) b
validateDimension (MkDimension a) = MkDimension a

combine2D :: (a -> b -> c) -> Dimension tag1 a -> Dimension tag2 b -> Dimension (UnZero (Merge tag1 tag2)) c 
combine2D f (MkDimension a) (MkDimension b) = MkDimension (f a b)

(!*) :: Num n => Dimension a n -> Dimension b n -> Dimension (UnZero (Merge a b)) n
(MkDimension a) !* (MkDimension b) = MkDimension (a * b)
infixl 7 !*
(!/) :: Fractional n => Dimension a n -> Dimension b n -> Dimension (UnZero (Merge a (Invert b))) n
(MkDimension a) !/ (MkDimension b) = MkDimension (a * b)
infixl 7 !/
divD :: Integral n => Dimension a n -> Dimension b n -> Dimension (UnZero (Merge a (Invert b))) n
divD (MkDimension a) (MkDimension b) = MkDimension (a `div` b)
undimension :: Dimension '[] a -> a
undimension (MkDimension a) = a

transform :: forall s t x a. TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transform (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of 
        EQ -> MkDimension a
        GT -> MkDimension $ appEndo (stimes times (Endo fun)) a
        LT -> MkDimension $ appEndo (stimes (negate times) (Endo invfun)) a

transformpos :: forall s t x a. (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformpos fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ appEndo (stimes times (Endo fun)) a

same :: forall s t x. (forall a. Dimension x a -> Dimension (Replace s t x) a)
same (MkDimension a) = MkDimension a

apply :: forall x a. forall s -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Delete s x) a
apply s (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of
        EQ -> MkDimension a
        GT -> MkDimension $ appEndo (stimes times (Endo fun)) a
        LT -> MkDimension $ appEndo (stimes (negate times) (Endo invfun)) a

applypos :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applypos s fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ appEndo (stimes times (Endo fun)) a
--mkisos is the same as repeated use of same
mkisos :: forall y x a. Dimension x a -> Dimension (Isos y x) a
mkisos (MkDimension a) = MkDimension a
