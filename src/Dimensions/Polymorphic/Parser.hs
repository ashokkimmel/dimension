{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies,DataKinds,UndecidableInstances,AllowAmbiguousTypes #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
module Dimensions.Polymorphic.Parser (Parse,ToDimension,Read,ReadTypeDimension) where
import Dimensions.TypeLevelInt qualified as TI 
import Dimensions.TypeLevelInt (Int')
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.TypeLits qualified as TL 
import GHC.TypeLits (Symbol,Nat)
import Dimensions.Polymorphic.ParseMisc (IgnoreWhitespace, TakeWhileNotIn, UnUnConsSymbol, UnJust, StrToNat, NonEmptyToList, FToNegInt, FToPosInt, TrimWhitespaceRight)
import Data.Kind (Type)
type ToDimension :: forall a. Symbol -> a  -- This creates polymorphism in the parser
type family ToDimension b 
type instance ToDimension b = b
type instance ToDimension a = ReadTypeDimension a 

type ReadTypeDimension :: Symbol -> Type
type family ReadTypeDimension a 
-- Name scheme: 
-- W = Whitespace sensitive
-- U = Result of UnconsSymbol
-- A = Accumulator
-- N = Nonempty
-- Misc. Other matching uses P, H, or ' 
type Parse :: Symbol -> [(k, Int')]
type family Parse a where 
    Parse a = WParse (IgnoreWhitespace a) --ParseH (Parse1Word (IgnoreWhitespace a))
type WParse :: Symbol -> [(k, Int')]
type family WParse a where
    WParse "" = '[] 
    WParse a = NonEmptyToList (NParse a)

type NParse :: Symbol -> NonEmpty (k, Int')
type family NParse a where
    NParse a = NParseH (Parse1Word a)
type NParseH :: ((k, Int'), Symbol) -> NonEmpty (k, Int')
type family NParseH a where
    NParseH '(a, "") = a ':| '[] 
    NParseH '(a,b) = a ':| NonEmptyToList (ParseWithOperator (UnJust (TL.UnconsSymbol b))) -- Safe use of UnJust because of the "" case above
type ParseWithOperator :: (Char,Symbol) -> NonEmpty (k, Int')
type family ParseWithOperator a where 
    ParseWithOperator '( '*',a) = NParse (IgnoreWhitespace a)
    ParseWithOperator '( '/',a) = NegateFirst (NParse (IgnoreWhitespace a))
type NegateFirst :: NonEmpty (k, Int') -> NonEmpty (k, Int')
type family NegateFirst a where
    NegateFirst ( '(x,y) ':| xs) = '(x, TI.Negate y) ':| xs

type Parse1Word :: Symbol -> ((k, Int'), Symbol)
type family Parse1Word a where
    Parse1Word a = Parse1WordH (TakeWhileNotIn '[ '*', '^', '/'] a)
type Parse1WordH :: (Symbol, Symbol) -> ((k, Int'), Symbol)
type family Parse1WordH a where
    Parse1WordH '(a,b) = UnAssoc '(ToDimension (TrimWhitespaceRight a), GetExp b)
type UnAssoc :: (a,(b,c)) -> ((a,b),c)
type family UnAssoc a where 
    UnAssoc '(a,'(b,c)) = '( '(a,b),c)

type GetExp :: Symbol -> (Int', Symbol)
type family GetExp a where
    GetExp a = UGetExp (TL.UnconsSymbol a)
type UGetExp :: Maybe (Char, Symbol) -> (Int', Symbol)
type family UGetExp a where
    UGetExp ('Just '( '^', xs)) = ParseExp xs
    UGetExp a = '(TI.ToPosInt 1, UnUnConsSymbol a)
type ParseExp :: Symbol -> (Int', Symbol)
type family ParseExp a where
    ParseExp a = WParseExp (IgnoreWhitespace a)
type WParseExp :: Symbol -> (Int', Symbol)
type family WParseExp a where
    WParseExp a = UParseExp (TL.UnconsSymbol a)
type UParseExp :: Maybe (Char, Symbol) -> (Int', Symbol)
type family UParseExp a where
    UParseExp ('Just '( '-', xs)) = FToNegInt (ParseExpH xs)
    UParseExp ('Just '( '+', xs)) = FToPosInt (ParseExpH xs) -- Explicitly allow '+' for symmetry
    UParseExp a = FToPosInt (ParseExpH (UnUnConsSymbol a))

type ParseExpH :: Symbol -> (Nat, Symbol)
type family ParseExpH a where
    ParseExpH a = ParseExpH' (TakeWhileNotIn '[ ' ', '/', '*'] a)
type ParseExpH' :: (Symbol, Symbol) -> (Nat, Symbol)
type family ParseExpH' a where
    ParseExpH' '( a,b) = '(StrToNat a, IgnoreWhitespace b)

--type (<~~>) :: a -> a -> Constraint -- General checking type equality with custom error message
--type family (<~~>) a b where 
--    a <~~> a = ()
--    a <~~> b = TypeError ('Text "Expected " ':<>: 'TL.ShowType a 
--                         ':$$: 'Text " but got " ':<>: 'ShowType b)
--
--type (<~>) :: Symbol -> [(Symbol, Int')] -> Constraint -- Specialized for parse
--type family (<~>) a b where 
--    a <~> b = SpecializedParseCheck a (Parse a) b
--type SpecializedParseCheck :: Symbol -> [(k, Int')] -> [(k, Int')] -> Constraint
--type family SpecializedParseCheck a b c where 
--    SpecializedParseCheck _ b b = ()
--    SpecializedParseCheck a b c = TypeError ('Text "Parsing error: Expected " ':<>: 'TL.ShowType c
--                                            ':$$: 'Text " but got " ':<>: 'ShowType b 
--                                            ':$$: 'Text " when parsing the string " ':<>: 'TL.ShowType a)
--type Tests :: Constraint
--type Tests = 
--    ( "m" <~> '[ '("m", TI.ToPosInt 1)]
--    , "m^2" <~> '[ '("m",TI.ToPosInt 2)]
--    , "m^-2" <~> '[ '("m", TI.ToNegInt 2)]
--    , "m^+2" <~> '[ '("m", TI.ToPosInt 2)]
--    , " m ^ 2 " <~> '[ '("m", TI.ToPosInt 2)]
--    , "m^2 * s^-1" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
--    , "m^2/s" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
--    , " m ^ 2 / s " <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)]
--    , "kg*m^2/s^2" <~> '[ '("kg", TI.ToPosInt 1), '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 2)]
--    , "" <~> '[]
--    , "s" <~> '[ '("s", TI.ToPosInt 1)]
--    , "kg" <~> '[ '("kg", TI.ToPosInt 1)]
--    , "m * s" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToPosInt 1)]
--    , "m / s" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToNegInt 1)]
--    , "m^3 * s^-2 / kg" <~> '[ '("m", TI.ToPosInt 3), '("s", TI.ToNegInt 2), '("kg", TI.ToNegInt 1)]
--    , "m^0" <~> '[ '("m", TI.ToPosInt 0)]
--    , "m^1" <~> '[ '("m", TI.ToPosInt 1)]
--    , "m^+1" <~> '[ '("m", TI.ToPosInt 1)]
--    , "m^-1" <~> '[ '("m", TI.ToNegInt 1)]
--    , "m * s^2 * kg^-3" <~> '[ '("m", TI.ToPosInt 1), '("s", TI.ToPosInt 2), '("kg", TI.ToNegInt 3)]
--    , "m^2 * s / kg^-4" <~> '[ '("m", TI.ToPosInt 2), '("s", TI.ToPosInt 1), '("kg", TI.ToPosInt 4)]
--    )
--
--main :: Tests => IO ()
--main = pure ()
--
--