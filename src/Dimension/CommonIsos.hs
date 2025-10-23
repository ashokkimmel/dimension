{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
module Dimensions.CommonIsos (AbbrToFull) where
import GHC.TypeLits (Symbol)

type AbbrToFull :: [(Symbol, Symbol)]
type AbbrToFull = 
    '[
    '("m", "meter"),
    '("s", "second"),
    '("kg", "kilogram"),
    '("A", "ampere"),
    '("K", "kelvin"),
    '("mol", "mole"),
    '("cd", "candela"),
    '("rad", "radian"),
    '("sr", "steradian"),
    '("Hz", "hertz"),
    '("N", "newton"),
    '("Pa", "pascal"),
    '("J", "joule"),
    '("W", "watt"),
    '("C", "coulomb"),
    '("V", "volt"),
    '("F", "farad"),
    '("Ω", "ohm"),
    '("S", "siemens"),
    '("Wb", "weber"),
    '("T", "tesla"),
    '("H", "henry"),
    '("lm", "lumen"),
    '("lx", "lux"),
    '("Bq", "becquerel"),
    '("Gy", "gray"),
    '("Sv", "sievert"),
    '("kat", "katal"),
    '("eV", "electronvolt"),
    '("u", "atomic mass unit"),
    '("au", "astronomical unit"),
    '("ly", "light-year"),
    '("pc", "parsec"),
    '("min", "minute"),
    '("h", "hour"),
    '("d", "day"),
    '("°", "degree"),
    '("'", "minute of arc"),
    '("\"", "second of arc")]
