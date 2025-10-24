import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol)
import Dimensions.TypeMisc (UnConsSymbol)
data Kilo = Kilo
type StripPrefix :: Symbol -> Symbol -> Maybe Symbol
type family StripPrefix a b where 
    StripPrefix a b = StripPrefixU (TL.UnconsSymbol a) (TL.UnconsSymbol b)
type StripPrefixU :: Maybe (Char, Symbol) -> Maybe (Char, Symbol) -> Maybe Symbol
type family StripPrefixU a b where
    StripPrefixU 'Nothing b = 'Just (UnUnConsSymbol b)
    StripPrefixU _ 'Nothing = 'Nothing
    StripPrefixU ('Just '(a,as)) ('Just '(a,bs)) = StripPrefixU (TL.UnconsSymbol as) (TL.UnconsSymbol bs)
    StripPrefixU _ _ = 'Nothing
instance Num b => MatchSome Kilo String b where 
    type MatchSome a = StripPrefix "kilo" a 
    convert = (*1e3)
instance Fractional b => MatchAll Kilo String b where 
    convert = (/1e3)