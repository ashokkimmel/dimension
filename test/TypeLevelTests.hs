{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
module TypeLevelTests where

import Data.Kind (Constraint)
import Dimensions.Parser (Parse)
import qualified Dimensions.TypeLevelInt as TI
import Dimensions.ParseMisc (StrToNat)
import Dimensions.Order (Length, Sort, Merge)
import Dimensions.DimensionalMisc (Delete, UnZero, Replace', Invert, LookupD0)
import Dimensions.Printer (Print)

-- Compile-time assertions as Constraint-kind type synonyms.
-- These aliases will cause GHC to solve the underlying equality
-- constraints when the combined `TypeLevelTests` synonym is used
-- (for example by building the package or importing this module
-- with a constraint that requires `TypeLevelTests`).

type AssertParseM :: Constraint
type AssertParseM = (Parse "m" ~ '[ '("m", TI.ToPosInt 1)])

type AssertParseM2 :: Constraint
type AssertParseM2 = (Parse "m^2" ~ '[ '("m", TI.ToPosInt 2)])

type AssertParseNeg :: Constraint
type AssertParseNeg = (Parse "m^-2" ~ '[ '("m", TI.ToNegInt 2)])

type AssertParseEmpty :: Constraint
type AssertParseEmpty = (Parse "" ~ '[])

type AssertStrToNat :: Constraint
type AssertStrToNat = (StrToNat "123" ~ 123)

type AssertLength3 :: Constraint
type AssertLength3 = (Length '["a","b","c"] ~ 3)

type AssertTliToNeg :: Constraint
type AssertTliToNeg = (TI.ToNegInt 2 ~ 'TI.Neg 1)

type AssertTliPlus :: Constraint
type AssertTliPlus = (( 'TI.Pos 2) TI.+ ( 'TI.Pos 3) ~ 'TI.Pos 5)

-- More TypeLevelInt tests
type AssertTliMinus :: Constraint
type AssertTliMinus = (( 'TI.Pos 10) TI.- ( 'TI.Pos 3) ~ 'TI.Pos 7)

type AssertTliMult :: Constraint
type AssertTliMult = (( 'TI.Pos 4) TI.* ( 'TI.Pos 5) ~ 'TI.Pos 20)

type AssertTliDiv :: Constraint
type AssertTliDiv = (( 'TI.Pos 12) TI./ ( 'TI.Pos 3) ~ 'TI.Pos 4)

type AssertTliNegate :: Constraint
type AssertTliNegate = (TI.Negate ( 'TI.Pos 5) ~ 'TI.Neg 4)

type AssertTliNegPlus :: Constraint
type AssertTliNegPlus = (( 'TI.Neg 2) TI.+ ( 'TI.Neg 3) ~ 'TI.Neg 6)

-- Parse tests for complex expressions
type AssertParseMultiple :: Constraint
type AssertParseMultiple = (Parse "m^2 * s^-1" ~ '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)])

type AssertParseDiv :: Constraint
type AssertParseDiv = (Parse "m^2/s" ~ '[ '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 1)])

type AssertParseComplex :: Constraint
type AssertParseComplex = (Parse "kg*m^2/s^2" ~ '[ '("kg", TI.ToPosInt 1), '("m", TI.ToPosInt 2), '("s", TI.ToNegInt 2)])

-- Sort tests
type AssertSort1 :: Constraint
type AssertSort1 = (Sort '[ '("z", 'TI.Pos 1), '("a", 'TI.Pos 2)] ~ '[ '("a", 'TI.Pos 2), '("z", 'TI.Pos 1)])

type AssertSort2 :: Constraint
type AssertSort2 = (Sort '[ '("m", 'TI.Pos 1)] ~ '[ '("m", 'TI.Pos 1)])

-- Merge tests
type AssertMerge1 :: Constraint
type AssertMerge1 = (Merge '[ '("a", 'TI.Pos 1)] '[ '("a", 'TI.Pos 2)] ~ '[ '("a", 'TI.Pos 3)])

type AssertMerge2 :: Constraint
type AssertMerge2 = (Merge '[ '("m", 'TI.Pos 2)] '[ '("s", 'TI.Pos 1)] ~ '[ '("m", 'TI.Pos 2), '("s", 'TI.Pos 1)])

-- UnZero tests
type AssertUnZero1 :: Constraint
type AssertUnZero1 = (UnZero '[ '("m", 'TI.Pos 0), '("s", 'TI.Pos 1)] ~ '[ '("s", 'TI.Pos 1)])

type AssertUnZero2 :: Constraint
type AssertUnZero2 = (UnZero '[ '("m", 'TI.Pos 1)] ~ '[ '("m", 'TI.Pos 1)])

-- Delete tests
type AssertDelete1 :: Constraint
type AssertDelete1 = (Delete "m" '[ '("m", 'TI.Pos 1), '("s", 'TI.Pos 2)] ~ '[ '("s", 'TI.Pos 2)])

type AssertDelete2 :: Constraint
type AssertDelete2 = (Delete "x" '[ '("m", 'TI.Pos 1), '("s", 'TI.Pos 2)] ~ '[ '("m", 'TI.Pos 1), '("s", 'TI.Pos 2)])

-- Replace' tests
type AssertReplace :: Constraint
type AssertReplace = (Replace' "m" "meter" '[ '("m", 'TI.Pos 1), '("s", 'TI.Pos 2)] ~ '[ '("meter", 'TI.Pos 1), '("s", 'TI.Pos 2)])

-- Invert tests
type AssertInvert1 :: Constraint
type AssertInvert1 = (Invert '[ '("m", 'TI.Pos 2)] ~ '[ '("m", 'TI.Neg 1)])

type AssertInvert2 :: Constraint
type AssertInvert2 = (Invert '[ '("m", 'TI.Pos 1), '("s", 'TI.Neg 1)] ~ '[ '("m", 'TI.Neg 0), '("s", 'TI.Pos 2)])

-- LookupD0 tests
type AssertLookup1 :: Constraint
type AssertLookup1 = (LookupD0 "m" '[ '("m", 'TI.Pos 3)] ~ 'TI.Pos 3)

type AssertLookup2 :: Constraint
type AssertLookup2 = (LookupD0 "x" '[ '("m", 'TI.Pos 3)] ~ 'TI.Pos 0)

-- Print tests
type AssertPrint1 :: Constraint
type AssertPrint1 = (Print '[ '("m", 'TI.Pos 1)] ~ "m")

type AssertPrint2 :: Constraint
type AssertPrint2 = (Print '[ '("m", 'TI.Pos 2)] ~ "m ^ 2")

type AssertPrint3 :: Constraint
type AssertPrint3 = (Print '[] ~ "")

-- StrToNat additional tests
type AssertStrToNat2 :: Constraint
type AssertStrToNat2 = (StrToNat "0" ~ 0)

type AssertStrToNat3 :: Constraint
type AssertStrToNat3 = (StrToNat "999" ~ 999)

-- Length additional tests
type AssertLength0 :: Constraint
type AssertLength0 = (Length '[] ~ 0)

type AssertLength1 :: Constraint
type AssertLength1 = (Length '["x"] ~ 1)

-- More comprehensive Parse tests with special characters and longer strings
type AssertParseKilogram :: Constraint
type AssertParseKilogram = (Parse "kilogram" ~ '[ '("kilogram", TI.ToPosInt 1)])

type AssertParseAmpere :: Constraint
type AssertParseAmpere = (Parse "ampere^3" ~ '[ '("ampere", TI.ToPosInt 3)])

type AssertParseUnderscore :: Constraint
type AssertParseUnderscore = (Parse "my_unit" ~ '[ '("my_unit", TI.ToPosInt 1)])

type AssertParseCapitals :: Constraint
type AssertParseCapitals = (Parse "Newton" ~ '[ '("Newton", TI.ToPosInt 1)])

type AssertParseMixedCase :: Constraint
type AssertParseMixedCase = (Parse "MyUnit^2" ~ '[ '("MyUnit", TI.ToPosInt 2)])

type AssertParseNumbers :: Constraint
type AssertParseNumbers = (Parse "unit123" ~ '[ '("unit123", TI.ToPosInt 1)])

type AssertParseLongExpression :: Constraint
type AssertParseLongExpression = (Parse "meter*second*kilogram" ~ '[ '("meter", TI.ToPosInt 1), '("second", TI.ToPosInt 1), '("kilogram", TI.ToPosInt 1)])

type AssertParseComplexMixed :: Constraint
type AssertParseComplexMixed = (Parse "kg^3*m^-2/s^4" ~ '[ '("kg", TI.ToPosInt 3), '("m", TI.ToNegInt 2), '("s", TI.ToNegInt 4)])

type AssertParseWithSpaces :: Constraint
type AssertParseWithSpaces = (Parse " m ^ 3 * s ^ -2 " ~ '[ '("m", TI.ToPosInt 3), '("s", TI.ToNegInt 2)])

type AssertParseHighExponent :: Constraint
type AssertParseHighExponent = (Parse "x^10" ~ '[ '("x", TI.ToPosInt 10)])

type AssertParseNegativeHighExp :: Constraint
type AssertParseNegativeHighExp = (Parse "y^-15" ~ '[ '("y", TI.ToNegInt 15)])

-- TypeLevelInt tests with larger numbers and edge cases
type AssertTliLargeAdd :: Constraint
type AssertTliLargeAdd = (( 'TI.Pos 100) TI.+ ( 'TI.Pos 250) ~ 'TI.Pos 350)

type AssertTliLargeMult :: Constraint
type AssertTliLargeMult = (( 'TI.Pos 12) TI.* ( 'TI.Pos 12) ~ 'TI.Pos 144)

type AssertTliMixedSignAdd :: Constraint
type AssertTliMixedSignAdd = (( 'TI.Pos 10) TI.+ ( 'TI.Neg 3) ~ 'TI.Pos 6)

type AssertTliMixedSignSub :: Constraint
type AssertTliMixedSignSub = (( 'TI.Pos 5) TI.- ( 'TI.Neg 2) ~ 'TI.Pos 8)

type AssertTliNegMult :: Constraint
type AssertTliNegMult = (( 'TI.Neg 2) TI.* ( 'TI.Neg 3) ~ 'TI.Pos 12)

type AssertTliZeroMult :: Constraint
type AssertTliZeroMult = (( 'TI.Pos 0) TI.* ( 'TI.Pos 999) ~ 'TI.Pos 0)

type AssertTliNegateZero :: Constraint
type AssertTliNegateZero = (TI.Negate ( 'TI.Pos 0) ~ 'TI.Pos 0)

type AssertTliNegateNeg :: Constraint
type AssertTliNegateNeg = (TI.Negate ( 'TI.Neg 5) ~ 'TI.Pos 6)

-- Sort tests with longer lists
type AssertSortThree :: Constraint
type AssertSortThree = (Sort '[ '("z", 'TI.Pos 1), '("a", 'TI.Pos 2), '("m", 'TI.Pos 3)] ~ '[ '("a", 'TI.Pos 2), '("m", 'TI.Pos 3), '("z", 'TI.Pos 1)])

type AssertSortEmpty :: Constraint
type AssertSortEmpty = (Sort '[] ~ '[])

-- Merge tests with cancellation
type AssertMergeCancellation :: Constraint
type AssertMergeCancellation = (Merge '[ '("m", 'TI.Pos 2)] '[ '("m", 'TI.Neg 1)] ~ '[ '("m", 'TI.Pos 1)])

type AssertMergeEmpty :: Constraint
type AssertMergeEmpty = (Merge '[] '[] ~ '[])

type AssertMergeComplex :: Constraint
type AssertMergeComplex = (Merge '[ '("a", 'TI.Pos 1), '("c", 'TI.Pos 2)] '[ '("b", 'TI.Pos 3), '("c", 'TI.Pos 1)] ~ '[ '("a", 'TI.Pos 1), '("b", 'TI.Pos 3), '("c", 'TI.Pos 3)])

-- Delete tests with longer dimension names
type AssertDeleteLongName :: Constraint
type AssertDeleteLongName = (Delete "kilogram" '[ '("kilogram", 'TI.Pos 2), '("meter", 'TI.Pos 1)] ~ '[ '("meter", 'TI.Pos 1)])

type AssertDeleteAll :: Constraint
type AssertDeleteAll = (Delete "x" '[ '("x", 'TI.Pos 1), '("x", 'TI.Pos 2), '("x", 'TI.Neg 1)] ~ '[])

-- Replace' tests with special characters
type AssertReplaceUnderscore :: Constraint
type AssertReplaceUnderscore = (Replace' "old_unit" "new_unit" '[ '("old_unit", 'TI.Pos 1)] ~ '[ '("new_unit", 'TI.Pos 1)])

type AssertReplaceMultiple :: Constraint
type AssertReplaceMultiple = (Replace' "m" "meter" '[ '("m", 'TI.Pos 1), '("m", 'TI.Pos 2), '("s", 'TI.Pos 1)] ~ '[ '("meter", 'TI.Pos 1), '("meter", 'TI.Pos 2), '("s", 'TI.Pos 1)])

-- Invert tests with complex lists
type AssertInvertEmpty :: Constraint
type AssertInvertEmpty = (Invert '[] ~ '[])

type AssertInvertComplex :: Constraint
type AssertInvertComplex = (Invert '[ '("a", 'TI.Pos 3), '("b", 'TI.Neg 2), '("c", 'TI.Pos 1)] ~ '[ '("a", 'TI.Neg 2), '("b", 'TI.Pos 3), '("c", 'TI.Neg 0)])

-- LookupD0 tests with longer names
type AssertLookupLongName :: Constraint
type AssertLookupLongName = (LookupD0 "kilogram" '[ '("kilogram", 'TI.Pos 5), '("meter", 'TI.Pos 1)] ~ 'TI.Pos 5)

type AssertLookupNegative :: Constraint
type AssertLookupNegative = (LookupD0 "s" '[ '("s", 'TI.Neg 3)] ~ 'TI.Neg 3)

-- Print tests with longer names and complex expressions
type AssertPrintLongName :: Constraint
type AssertPrintLongName = (Print '[ '("kilogram", 'TI.Pos 1)] ~ "kilogram")

type AssertPrintNegative :: Constraint
type AssertPrintNegative = (Print '[ '("meter", 'TI.Neg 1)] ~ "meter ^ -1")

type AssertPrintComplex :: Constraint
type AssertPrintComplex = (Print '[ '("kg", 'TI.Pos 2), '("m", 'TI.Pos 1)] ~ "kg ^ 2 * m")

type AssertPrintMixed :: Constraint
type AssertPrintMixed = (Print '[ '("N", 'TI.Pos 1), '("s", 'TI.Neg 2)] ~ "N / s ^ 2")

-- StrToNat tests with edge cases
type AssertStrToNat1 :: Constraint
type AssertStrToNat1 = (StrToNat "1" ~ 1)

type AssertStrToNatLarge :: Constraint
type AssertStrToNatLarge = (StrToNat "12345" ~ 12345)

-- UnZero tests with multiple zeros
type AssertUnZeroMultiple :: Constraint
type AssertUnZeroMultiple = (UnZero '[ '("a", 'TI.Pos 0), '("b", 'TI.Pos 1), '("c", 'TI.Pos 0)] ~ '[ '("b", 'TI.Pos 1)])

type AssertUnZeroAllZero :: Constraint
type AssertUnZeroAllZero = (UnZero '[ '("x", 'TI.Pos 0), '("y", 'TI.Pos 0)] ~ '[])

type AssertUnZeroNone :: Constraint
type AssertUnZeroNone = (UnZero '[ '("a", 'TI.Pos 1), '("b", 'TI.Neg 2)] ~ '[ '("a", 'TI.Pos 1), '("b", 'TI.Neg 2)])

-- Length tests with longer lists
type AssertLength5 :: Constraint
type AssertLength5 = (Length '["a","b","c","d","e"] ~ 5)

type AssertLength10 :: Constraint
type AssertLength10 = (Length '["1","2","3","4","5","6","7","8","9","10"] ~ 10)


-- Combined alias that can be used to force-check all assertions at once.
type TypeLevelTests :: Constraint
type TypeLevelTests = ( AssertParseM
                      , AssertParseM2
                      , AssertParseNeg
                      , AssertParseEmpty
                      , AssertParseMultiple
                      , AssertParseDiv
                      , AssertParseComplex
                      , AssertParseKilogram
                      , AssertParseAmpere
                      , AssertParseUnderscore
                      , AssertParseCapitals
                      , AssertParseMixedCase
                      , AssertParseNumbers
                      , AssertParseLongExpression
                      , AssertParseComplexMixed
                      , AssertParseWithSpaces
                      , AssertParseHighExponent
                      , AssertParseNegativeHighExp
                      , AssertStrToNat
                      , AssertStrToNat1
                      , AssertStrToNat2
                      , AssertStrToNat3
                      , AssertStrToNatLarge
                      , AssertLength0
                      , AssertLength1
                      , AssertLength3
                      , AssertLength5
                      , AssertLength10
                      , AssertTliToNeg
                      , AssertTliPlus
                      , AssertTliMinus
                      , AssertTliMult
                      , AssertTliDiv
                      , AssertTliNegate
                      , AssertTliNegPlus
                      , AssertTliLargeAdd
                      , AssertTliLargeMult
                      , AssertTliMixedSignAdd
                      , AssertTliMixedSignSub
                      , AssertTliNegMult
                      , AssertTliZeroMult
                      , AssertTliNegateZero
                      , AssertTliNegateNeg
                      , AssertSort1
                      , AssertSort2
                      , AssertSortThree
                      , AssertSortEmpty
                      , AssertMerge1
                      , AssertMerge2
                      , AssertMergeCancellation
                      , AssertMergeEmpty
                      , AssertMergeComplex
                      , AssertUnZero1
                      , AssertUnZero2
                      , AssertUnZeroMultiple
                      , AssertUnZeroAllZero
                      , AssertUnZeroNone
                      , AssertDelete1
                      , AssertDelete2
                      , AssertDeleteLongName
                      , AssertDeleteAll
                      , AssertReplace
                      , AssertReplaceUnderscore
                      , AssertReplaceMultiple
                      , AssertInvert1
                      , AssertInvert2
                      , AssertInvertEmpty
                      , AssertInvertComplex
                      , AssertLookup1
                      , AssertLookup2
                      , AssertLookupLongName
                      , AssertLookupNegative
                      , AssertPrint1
                      , AssertPrint2
                      , AssertPrint3
                      , AssertPrintLongName
                      , AssertPrintNegative
                      , AssertPrintComplex
                      , AssertPrintMixed
                      ) 

