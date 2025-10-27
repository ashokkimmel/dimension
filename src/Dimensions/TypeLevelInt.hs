{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
module Dimensions.TypeLevelInt (type (+),type (-), type (*),type (/),ToNegInt,ToPosInt,Negate,Int'(Pos,Neg),ToNatural) where
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Nat)
import GHC.TypeError ( TypeError, ErrorMessage(Text,ShowType,(:<>:)) )
import Dimensions.TypeMisc (IfThenElse)
import Data.Kind (Type)
type Int'     :: Type
data Int' = Pos Nat | Neg Nat -- Neg n represents -(n+1)
type (+) :: Int' -> Int' -> Int'
type family (+) m n where
    ('Pos m) + ('Pos n) = 'Pos (m TL.+ n)
    ('Neg m) + ('Neg n) = 'Neg (m TL.+ n TL.+ 1)
    ('Pos m) + ('Neg n) = IfThenElse (m TL.<=? n) ('Neg (n TL.- m)) ('Pos (m TL.- n TL.- 1))
    ('Neg m) + ('Pos n) = IfThenElse (n TL.<=? m) ('Neg (m TL.- n)) ('Pos (n TL.- m TL.- 1))
type (-) :: Int' -> Int' -> Int'
type family (-) m n where
    ('Pos m) - ('Pos n) = IfThenElse (n TL.<=? m) ('Pos (m TL.- n)) ('Neg (n TL.- m TL.- 1))
    ('Neg n) - ('Neg m) = IfThenElse (n TL.<=? m) ('Pos (m TL.- n)) ('Neg (n TL.- m TL.- 1))
    ('Pos m) - ('Neg n) = 'Pos (m TL.+ n TL.+ 1)
    ('Neg m) - ('Pos n) = 'Neg (m TL.+ n)
type ToNatural :: Int' -> Nat
type family ToNatural (n :: Int') :: Nat where
    ToNatural ('Pos m) = m
type (*) :: Int' -> Int' -> Int'
type family (*) m n where
    ('Pos 0) * _ = 'Pos 0
    _ * ('Pos 0) = 'Pos 0
    ('Pos m) * ('Pos n) = 'Pos (m TL.* n)
    ('Neg m) * ('Neg n) = 'Pos ((m TL.+ 1) TL.* (n TL.+ 1))
    ('Pos m) * ('Neg n) = 'Neg (m TL.* (n TL.+ 1) TL.- 1)
    ('Neg n) * ('Pos m) = 'Neg (m TL.* (n TL.+ 1) TL.- 1)
type (/) :: Int' -> Int' -> Int'
type family (/) a b where 
    ('Pos a) / ('Pos b) = 'Pos (a `SafeDiv` b)
    ('Pos a) / ('Neg b) = ToNegInt (a `SafeDiv` (b TL.+ 1))
    ('Neg a) / ('Pos b) = ToNegInt ((1 TL.+ a) `SafeDiv` b)
    ('Neg a) / ('Neg b) = 'Pos ((1 TL.+ a) `SafeDiv` (b TL.+ 1))

type Negate :: Int' -> Int'
type family Negate a where
    Negate ('Pos 0) = 'Pos 0
    Negate ('Pos n) = 'Neg (n TL.- 1)
    Negate ('Neg n) = 'Pos (n TL.+ 1)

type ToNegInt :: Nat -> Int'
type family ToNegInt a where
    ToNegInt 0 = 'Pos 0
    ToNegInt n = 'Neg (n TL.- 1)

type ToPosInt :: Nat -> Int'
type family ToPosInt a where
    ToPosInt n = 'Pos n
type SafeDiv :: Nat -> Nat -> Nat
type family SafeDiv a b where
    SafeDiv a b = SafeDivH a b (a `TL.Mod` b) (a `TL.Div` b)
type SafeDivH :: Nat -> Nat -> Nat -> Nat -> Nat
type family SafeDivH x y a b where
    SafeDivH _ _ 0 b = b
    SafeDivH a b _ _ = TypeError ('Text "Remainder of " ':<>: 'ShowType a ':<>: 'Text " divided by " ':<>: 'ShowType b ':<>: 'Text " is non-zero, cannot convert to integer")
