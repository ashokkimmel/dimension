{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Dimensions.Ordered.Order (Sort,Merge) where 
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Nat)
import Dimensions.TypeLevelInt qualified as TI
import Dimensions.TypeLevelInt (Int')
import Data.Type.Ord (Compare)
type Sort :: [(k, Int')] -> [(k, Int')]
type family Sort a where
    Sort '[a] = '[a]
    Sort '[] = '[]
    Sort a = SortH (Halve a)
type SortH :: ([(k, Int')],[(k, Int')]) -> [(k, Int')]
type family SortH a where
    SortH '(a, b) = Merge (Sort a) (Sort b)
type Merge :: [(k, Int')] -> [(k, Int')] -> [(k, Int')]
type family Merge a b where
    Merge '[] a = a
    Merge a '[] = a
    Merge ('(a,a') ': c) ('(b,b') ': d) = MergeH (a `Compare` b) '(a,a') '(b,b') c d
type MergeH :: Ordering -> (k, Int') -> (k, Int') -> [(k, Int')] -> [(k, Int')] -> [(k, Int')]
type family MergeH ord a b c d where
    MergeH 'EQ '(a,b) '(_,d) e f = '(a,b TI.+ d) ': Merge e f
    MergeH 'LT a b c d = a ': Merge c (b ': d)
    MergeH 'GT a b c d = b ': Merge (a ': c)  d


type Halve :: [a] -> ([a],[a])
type family Halve a where
    Halve a = Span (Length a `TL.Div` 2) a

type Span :: Nat -> [a] -> ([a],[a])
type family Span n xs where
    Span 0 xs = '( '[], xs)
    Span n (x ': xs) = Add x (Span (n TL.- 1) xs)
type Add :: a -> ([a],b) -> ([a],b)
type family Add a b where
    Add a '(d,c) = '(a ': d,c)

type Length :: [a] -> Nat
type family Length xs where
    Length '[] = 0
    Length (_ ': xs) = 1 TL.+ Length xs
