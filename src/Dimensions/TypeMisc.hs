{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
module Dimensions.TypeMisc (type (<>),Maybe',FoundZero,IfThenElse,type (==)) where
import GHC.TypeLits qualified as TL
import GHC.TypeLits (Symbol)
import GHC.TypeError ( TypeError, ErrorMessage(Text) )
type FoundZero :: k 
type FoundZero @k = TypeError ('Text "There should not be a zero exponent in a dimension, please report this as a bug.")
type (<>) :: Symbol -> Symbol -> Symbol
type a <> b = a `TL.AppendSymbol` b
type Maybe' :: a -> Maybe a -> a
type family Maybe' a b where 
    Maybe' b 'Nothing  = b
    Maybe' _ ('Just a) = a
type IfThenElse :: Bool -> a -> a -> a
type family IfThenElse cond a b where
  IfThenElse 'True  a _ = a
  IfThenElse 'False _ b = b
type (==) :: a -> a -> Bool
type family (a :: k) == (b :: k) where
    a == a = 'True
    _ == _ = 'False