module Dimensions.Match (MatchPos(..), MatchAll(..)) where
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol)

type StripPrefix :: Symbol -> Symbol -> Maybe Symbol
type family StripPrefix a b where 
    StripPrefix a b = StripPrefixU (TL.UnconsSymbol a) (TL.UnconsSymbol b)
type StripPrefixU :: Maybe (Char, Symbol) -> Maybe (Char, Symbol) -> Maybe Symbol
type family StripPrefixU a b where
    StripPrefixU _ 'Nothing = 'Nothing
    StripPrefixU 'Nothing ('Just (b, bs)) = 'Just (TL.ConsSymbol b bs)
    StripPrefixU ('Just '(a,as)) ('Just '(a,bs)) = StripPrefixU (TL.UnconsSymbol as) (TL.UnconsSymbol bs)
    StripPrefixU _ _ = 'Nothing
data Kilo = Kilo
instance Num b => MatchSome Kilo String b where 
    type MatchSome a = StripPrefix "kilo" a 
    convert = (*1e3)
instance Fractional b => MatchAll Kilo String b where 
    convert = (/1e3)
type ChangeMatch :: identifier -> [(k,a)] -> [(k,a)] 
type family ChangeMatch identifier symbols where 
    ChangeMatch identifier '[] = '[] 
    ChangeMatch identifier ( '(s,v) ': xs) = ChangeMatchM (Match identifier s)
class MatchPos identifier k b | identifier -> k where 
    type Match identifier :: k -> Maybe k 
    convert :: b -> b 
class MatchPos identifier k b => MatchAll identifier k b | identifier -> k where 
    unconvert :: b -> b 
