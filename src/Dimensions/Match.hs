{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}

module Dimensions.Match (MatchPos(..), MatchAll(..),ChangeMatch,StripPrefix,HowManyMatches) where
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol)
import Data.Kind (Constraint,Type)
import Dimensions.TypeMisc (IfThenElse,type (==))
import qualified Dimensions.TypeLevelInt  as TI
import Dimensions.TypeLevelInt (Int')
type StripPrefix :: Symbol -> Symbol -> Maybe Symbol
type family StripPrefix a b where 
    StripPrefix a b = StripPrefixU (TL.UnconsSymbol a) (TL.UnconsSymbol b)
type StripPrefixU :: Maybe (Char, Symbol) -> Maybe (Char, Symbol) -> Maybe Symbol
type family StripPrefixU a b where
    StripPrefixU _ 'Nothing = 'Nothing
    StripPrefixU 'Nothing ('Just '(b, bs)) = 'Just (TL.ConsSymbol b bs)
    StripPrefixU ('Just '(a,as)) ('Just '(a,bs)) = StripPrefixU (TL.UnconsSymbol as) (TL.UnconsSymbol bs)
    StripPrefixU _ _ = 'Nothing

type ChangeMatch :: identifier -> [(k,a)] -> [(k,a)] 
type family ChangeMatch identifier symbols where 
    ChangeMatch _ '[] = '[] 
    ChangeMatch identifier ( '(s,v) ': xs) = ChangeMatchM (Match identifier s) v identifier xs
type HowManyMatches :: identifier -> [(k,a)] -> Int' 
type family HowManyMatches identifier xs where 
    HowManyMatches _ '[] = 'TI.Pos 0
    HowManyMatches identifier ( '(s,v) ': xs) = 
        IfThenElse 
            (Match identifier s == 'Nothing)
            (HowManyMatches identifier xs) 
            (v TI.+ HowManyMatches identifier xs)
type ChangeMatchM :: Maybe k -> a -> [(k,a)] -> identifier -> [(k,a)]
type family ChangeMatchM match amount identifier xs where 
    ChangeMatchM 'Nothing _ identifier xs =  ChangeMatch identifier xs
    ChangeMatchM ('Just s) v identifier xs = '(s,v) ': ChangeMatch identifier xs
type MatchPos :: identifier -> Type -> Type -> Constraint
class MatchPos identifier k b | identifier -> k where 
    type Match identifier :: k -> Maybe k 
    convert :: b -> b 
type MatchAll :: identifier -> Type -> Type -> Constraint
class MatchPos identifier k b => MatchAll identifier k b | identifier -> k where 
    unconvert :: b -> b 
