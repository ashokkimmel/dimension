{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Dimensions.DimensionalMisc (Isos',Delete,UnZero,Replace',Lookup,LookupD0,Invert) where 
import Dimensions.TypeLevelInt qualified as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.TypeMisc (Maybe')
type Isos' :: [(a,a)] -> [(a,b)] -> [(b,c)]
type family Isos' a b where
    Isos' '[] a = a 
    Isos' ('(a,b)':c) d = Isos' c (Replace' a b d) 
type Delete :: a -> [(a,b)] -> [(a,b)]
type family Delete s x where 
    Delete _ '[] = '[]
    Delete a ('(a,_) ': c) = Delete a c
    Delete a (c ': d) = c ': Delete a d
type UnZero :: [(a,Int')] -> [(a,Int')]
type family UnZero a where
    UnZero '[] = '[]
    UnZero ('(_,'TI.Pos 0) ': b) = UnZero b
    UnZero (a ': b) = a ': UnZero b
type Replace' :: a -> a -> [(a, b)] -> [(a,b)]
type family Replace' s t x where
    Replace' _ _ '[] = '[]
    Replace' a b ('(a,c) ': d) = '(b,c) ': Replace' a b d
type Lookup :: a -> [(a,b)] -> Maybe b
type family Lookup a b where
    Lookup _ '[]  = 'Nothing
    Lookup a ('(a,b) ': _) = 'Just b
    Lookup a (_ ': b) = Lookup a b
type LookupD0 :: a -> [(a,Int')] -> Int' 
type family LookupD0 a b where
    LookupD0 a b = Maybe' ('TI.Pos 0) (Lookup a b) 
type Invert :: [(k, Int')] -> [(k, Int')]
type family Invert a where 
    Invert '[] = '[] 
    Invert ('(a,b) ': c) = '(a,TI.Negate b) ': Invert c 