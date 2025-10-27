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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module Dimensions.Units (Dimension(..)
                                , (!*)
                                , (!/)
                                , (!-)
                                , (!+)
                                , type (!*)
                                , type (!/)
                                , type (!^)
                                , type (!|^|)
                                , Replace
                                , Isos
                                , Delete
                                , Format
                                , ValidDimension
                                , ValidParse
                                , mkisos
                                , applypos
                                , applyneg
                                , apply
                                , same
                                , transformpos
                                , transformneg
                                , transform
                                , validateDimension
                                , undimension
                                , dimensions
                                , dimension
                                , dimensionsPoly
                                , dimensionPoly
                                , divD
                                , combineD2
                                , liftD2
                                , noParseDimensions
                                , noParseDimension
                                , getDimensionNoParse
                                , getDimension 
                                , dims 
                                , dim 
                                , combineInvD2) where 
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol,Nat)
import qualified Dimensions.TypeLevelInt as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.Parser (Parse)
import Dimensions.Order (Sort,Merge)
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
type (!*) :: [(k,Int')] -> [(k,Int')] -> [(k,Int')]
type (!*) a b = UnZero (Merge a b)
type (!/) :: [(k,Int')] -> [(k,Int')] -> [(k,Int')]
type (!/) a b = UnZero (Merge a (Invert b))
type (!^) :: [(a,Int')] -> Int' -> [(a,Int')]
type family (!^) a b where 
  '[] !^ _ = '[]
  ('(a,b)':xs) !^ e = '(a,b TI.* e) ': xs !^ e 
type (!|^|) :: [(a,Int')] -> Nat -> [(a,Int')]
type a !|^| b = a !^ ('TI.Pos b)
type RT :: [(a,Int')] ->  Int' -> [(a,Int')]
type family RT a b where 
  '[] `RT` _ = '[]
  ('(a,e)':xs) `RT` b = '(a, e TI./ b) ': xs `RT` b
(!+) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!+) = liftD2 (+)
infixl 6 !+
{-# INLINE (!+) #-}

(!-) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!-) = liftD2 (-)
infixl 6 !-
{-# INLINE (!-) #-}
dim :: forall b. b -> forall a -> Dimension (ValidParse @Symbol a) b 
dim b _ = MkDimension b  
{-# INLINE dim #-}
dims :: forall f b. Functor f => f b -> forall a ->  f (Dimension (ValidParse @Symbol a) b) 
dims b _ = fmap MkDimension b  
{-# INLINE dims #-}

dimension :: forall a -> forall b. b -> Dimension (ValidParse @Symbol a)  b
dimension _ = MkDimension
{-# INLINE dimension #-}

dimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse @Symbol a) b)
dimensions _ = fmap MkDimension
{-# INLINE dimensions #-}

dimensionPoly :: forall a -> forall b.  b -> Dimension (ValidParse a) b
dimensionPoly _ = MkDimension
{-# INLINE dimensionPoly #-}

dimensionsPoly :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse a) b)
dimensionsPoly _ = fmap MkDimension
{-# INLINE dimensionsPoly #-}

noParseDimension :: forall a -> forall b. b -> Dimension (Format a) b
noParseDimension _ = MkDimension
{-# INLINE noParseDimension #-}

noParseDimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (Format a) b)
noParseDimensions _ = fmap MkDimension
{-# INLINE noParseDimensions #-}

validateDimension :: Dimension a b -> Dimension (Format a) b
validateDimension (MkDimension a) = MkDimension a
{-# INLINE validateDimension #-}

combineD2 :: (a -> b -> c) -> Dimension tag1 a -> Dimension tag2 b -> Dimension (tag1 !* tag2) c
combineD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b)
{-# INLINE combineD2 #-}
combineInvD2 :: (a -> b -> c) -> Dimension tag1 a -> Dimension tag2 b -> Dimension (tag1 !/ tag2) c
combineInvD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b)
{-# INLINE combineInvD2 #-}

(!*) :: Num n => Dimension a n -> Dimension b n -> Dimension (a !* b) n
(MkDimension a) !* (MkDimension b) = MkDimension (a * b)
infixl 7 !*
{-# INLINE (!*) #-}
(!/) :: Fractional n => Dimension a n -> Dimension b n -> Dimension (a !/ b) n
(MkDimension a) !/ (MkDimension b) = MkDimension (a / b)
infixl 7 !/
{-# INLINE (!/) #-}
divD :: Integral n => Dimension a n -> Dimension b n -> Dimension (a !/ b) n
divD (MkDimension a) (MkDimension b) = MkDimension (a `div` b)
{-# INLINE divD #-}
undimension :: Dimension '[] a -> a
undimension (MkDimension a) = a
{-# INLINE undimension #-}
getDimension :: forall a -> Dimension (Parse a) c -> c 
getDimension _ (MkDimension c) = c
{-# INLINE getDimension #-}
getDimensionNoParse :: forall a -> Dimension a c -> c 
getDimensionNoParse _ (MkDimension c) = c
{-# INLINE getDimensionNoParse #-}
doN :: (Eq a, Num a) => (t -> t) -> a -> t -> t
doN f = go where 
    go 0 a = a
    go x a = f (go x a)
{-# INLINE doN #-}
transform :: forall x a. forall s t -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transform s _ (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of 
        EQ -> MkDimension a
        GT -> MkDimension $ doN fun times a
        LT -> MkDimension $ doN invfun (negate times) a
{-# INLINE transform #-}

transformpos :: forall x a. forall s t -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformpos s _ fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ doN fun times a
{-# INLINE transformpos #-}

transformneg :: forall x a. forall s t -> (TL.KnownNat (TI.ToNatural (TI.Negate (LookupD0 s x)))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformneg s _ fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (TI.Negate (LookupD0 s x))) in
    MkDimension $ doN fun times a
{-# INLINE transformneg #-}

same :: forall x a. forall s t -> Dimension x a -> Dimension (Replace s t x) a
same _ _ (MkDimension a) = MkDimension a
{-# INLINE same #-}

apply :: forall x a. forall s -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Delete s x) a
apply s (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of
        EQ -> MkDimension a
        GT -> MkDimension $ doN fun times  a
        LT -> MkDimension $ doN invfun (negate times) a
{-# INLINE apply #-}

applypos :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applypos s fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ doN fun times  a
{-# INLINE applypos #-}

applyneg :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (TI.Negate (LookupD0 s x)))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applyneg s fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (TI.Negate (LookupD0 s x))) in
    MkDimension $ doN fun times a
{-# INLINE applyneg #-}
--mkisos is the same as repeated use of same
mkisos :: forall y -> forall x a. Dimension x a -> Dimension (Isos y x) a
mkisos _ (MkDimension a) = MkDimension a
{-# INLINE mkisos #-}
