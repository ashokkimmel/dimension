{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Dimensions.ParseMisc 
                 (IgnoreWhitespace
                 ,TakeWhileNotIn
                 ,UnUnConsSymbol
                 ,UnJust
                 ,StrToNat
                 ,NonEmptyToList
                 ,FToNegInt
                 ,FToPosInt
                 ,ParseResultType
                 ,TrimWhitespaceRight) where
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Nat, Symbol)
import Dimensions.TypeLevelInt (Int')
import Dimensions.TypeLevelInt qualified as TI
import GHC.TypeError (TypeError,ErrorMessage((:<>:),Text,ShowType))
import Dimensions.TypeMisc (IfThenElse)
type IgnoreWhitespace :: Symbol -> Symbol
type family IgnoreWhitespace a where
  IgnoreWhitespace a = UIgnoreWhitespace (TL.UnconsSymbol a)

type UIgnoreWhitespace :: Maybe (Char, Symbol) -> Symbol
type family UIgnoreWhitespace a where
  UIgnoreWhitespace ('Just '( ' ', xs)) = IgnoreWhitespace xs
  UIgnoreWhitespace a = UnUnConsSymbol a

type UnUnConsSymbol :: Maybe (Char, Symbol) -> Symbol
type family UnUnConsSymbol a where
  UnUnConsSymbol ('Just '(a, b)) = TL.ConsSymbol a b
  UnUnConsSymbol 'Nothing = ""

type NonEmptyToList :: NonEmpty a -> [a]
type family NonEmptyToList a where
  NonEmptyToList (x ':| xs) = x ': xs

type ParseResultType :: Type -> Type 
type ParseResultType a = [(a,Int')]

type Elem :: a -> [a] -> Bool
type family Elem e es where
  Elem _ '[] = 'False
  Elem x (x ': _) = 'True
  Elem x (_ ': xs) = Elem x xs

type UnJust :: Maybe a -> a
type family UnJust a where
  UnJust ('Just a) = a

type TakeWhileNotIn :: [Char] -> Symbol -> (Symbol, Symbol)
type family TakeWhileNotIn chars a where
  TakeWhileNotIn _      "" = '("", "")
  TakeWhileNotIn chars sym = UTakeWhileNotIn chars (UnJust (TL.UnconsSymbol sym)) -- Safe use of UnJust because of the "" case above

type UTakeWhileNotIn :: [Char] -> (Char, Symbol) -> (Symbol, Symbol)
type family UTakeWhileNotIn chars a where
  UTakeWhileNotIn chars '(x, xs) =
    IfThenElse
      (Elem x chars)
      '("", TL.ConsSymbol x xs)
      (AddLeft x (TakeWhileNotIn chars xs))

type AddLeft :: Char -> (Symbol, a) -> (Symbol, a)
type family AddLeft a b where
  AddLeft a '(b, c) = '(TL.ConsSymbol a b, c)

type TrimWhitespaceRight :: Symbol -> Symbol
type family TrimWhitespaceRight a where
  TrimWhitespaceRight "" = ""
  TrimWhitespaceRight a = (TrimWhiteSpaceRight' (TakeWhileNotIn '[ ' '] a))
type TrimWhiteSpaceRight' :: (Symbol, Symbol) -> Symbol
type family TrimWhiteSpaceRight' a where
  TrimWhiteSpaceRight' '(a, b) = TrimWhitespaceRight'' a (Span ' ' b)
type TrimWhitespaceRight'' :: Symbol -> (Symbol, Symbol) -> Symbol
type family TrimWhitespaceRight'' a b where
    TrimWhitespaceRight'' a '(_, "") = a
    TrimWhitespaceRight'' a '(b, c) = TL.AppendSymbol a (TL.AppendSymbol b (TrimWhitespaceRight c))
type Span :: Char -> Symbol -> (Symbol, Symbol)
type family Span c a where
    Span c a = USpan c (TL.UnconsSymbol a)
type USpan :: Char -> Maybe (Char, Symbol) -> (Symbol, Symbol)
type family USpan c a where
    USpan _ 'Nothing = '("", "")
    USpan c ('Just '(c, xs)) = AddLeft c (Span c xs)
    USpan _ ('Just '(x, xs)) = '("", TL.ConsSymbol x xs)

type ToNumeral :: Char -> Nat 
type family ToNumeral a where
    ToNumeral '0' = 0
    ToNumeral '1' = 1
    ToNumeral '2' = 2
    ToNumeral '3' = 3
    ToNumeral '4' = 4
    ToNumeral '5' = 5
    ToNumeral '6' = 6
    ToNumeral '7' = 7
    ToNumeral '8' = 8
    ToNumeral '9' = 9
    ToNumeral  a  = TypeError ('Text "Invalid numeral character: " ':<>: 'ShowType a)
type StrToNat :: Symbol -> Nat 
type family StrToNat a where 
    StrToNat a = UStrToNat (TL.UnconsSymbol a)
type UStrToNat :: Maybe (Char, Symbol) -> Nat
type family UStrToNat a where
    UStrToNat 'Nothing = TypeError ('Text "Expected a numeral but got the empty string")
    UStrToNat ('Just '(x,xs)) = AStrToNat (ToNumeral x) xs
type AStrToNat :: Nat -> Symbol -> Nat
type family AStrToNat a b where
    AStrToNat a "" = a 
    AStrToNat a b = UAStrToNat a (UnJust (TL.UnconsSymbol b)) -- Safe use of UnJust because of the "" case above
type UAStrToNat :: Nat -> (Char, Symbol) -> Nat
type family UAStrToNat a b where
    UAStrToNat a '( ',',xs) = AStrToNat a xs -- Allow commas as thousands separators
    UAStrToNat a '( '.',xs) = AStrToNat a xs -- Allow periods as thousands separators as well (e.g. in Europe)
    UAStrToNat a '(x,xs) = AStrToNat (a TL.* 10 TL.+ ToNumeral x) xs



type FToNegInt :: (Nat,a) -> (Int',a)
type family FToNegInt a where
    FToNegInt '(a,b) = '(TI.ToNegInt a, b)

type FToPosInt :: (Nat,a) -> (Int',a)
type family FToPosInt a where
    FToPosInt '(a,b) = '(TI.ToPosInt a, b)