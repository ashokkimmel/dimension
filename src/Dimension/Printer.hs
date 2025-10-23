{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances,AllowAmbiguousTypes #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Dimensions.Printer (FromDimension,FromTypeDimension,Print) where 
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Symbol,Nat)
import Dimensions.TypeLevelInt qualified as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.TypeMisc (FoundZero,type (<>))
import Data.Kind (Type)
-- The generalized printer type family 
type FromDimension :: forall a. a -> Symbol
type family FromDimension b
type instance FromDimension b = b
type instance FromDimension b = FromTypeDimension b
-- The specialized printer for type-level parsed dimensions
type FromTypeDimension :: Type -> Symbol
type family FromTypeDimension a


type Print :: [(k,Int')] -> Symbol
type family Print a where
    Print '[] = ""
    Print ( '(_,'TI.Pos 0) ': _) = FoundZero
    Print ( '(s,'TI.Pos 1) ': xs) = s <> PrintH xs 
    Print ( '(s,'TI.Pos n) ': a) = FromDimension s <> " ^ " <> DisplayNat n <> PrintH a
    Print ( '(s,'TI.Neg n) ': a) = FromDimension s <> " ^ -" <> DisplayNat n <> PrintH a

type PrintH :: [(k,Int')] -> Symbol
type family PrintH a where
    PrintH '[] = ""
    PrintH ( '(_,'TI.Pos 0) ': _) = FoundZero
    PrintH ( '(s,'TI.Pos n) ': xs) = " * " <> FromDimension s <> PrintExp n <> PrintH xs
    PrintH ( '(s,'TI.Neg n) ': xs) = " / " <> FromDimension s <> PrintExp (n TL.+ 1) <> PrintH xs

type PrintExp :: Nat -> Symbol
type family PrintExp a where 
    PrintExp 0 = FoundZero
    PrintExp 1 = ""
    PrintExp n = " ^ " <> DisplayNat n

type DisplayNat :: Nat -> Symbol
type family DisplayNat a where
    DisplayNat 0 = "0"
    DisplayNat 1 = "1"
    DisplayNat 2 = "2"
    DisplayNat 3 = "3"
    DisplayNat 4 = "4"
    DisplayNat 5 = "5"
    DisplayNat 6 = "6"
    DisplayNat 7 = "7"
    DisplayNat 8 = "8"
    DisplayNat 9 = "9"
    DisplayNat a = DisplayNat (a `TL.Div` 10) <> DisplayNat (a `TL.Mod` 10)
