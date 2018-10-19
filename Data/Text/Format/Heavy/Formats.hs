{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains format descriptions for most used variable types.
module Data.Text.Format.Heavy.Formats where

import Data.Default
import Data.Typeable
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH.Lift
import Instances.TH.Lift

import Data.Text.Format.Heavy.Types

-- | Alignment of string within specified width
data Align = AlignLeft | AlignRight | AlignCenter
  deriving (Eq, Show)

$(deriveLift ''Align)

-- | Whether to show the sign of number
data Sign = Always | OnlyNegative | SpaceForPositive
  deriving (Eq, Show)

$(deriveLift ''Sign)

-- | Number base
data Radix = Decimal | Hexadecimal
  deriving (Eq, Show)

$(deriveLift ''Radix)

-- | Supported text conversions
data Conversion =
    UpperCase
  | LowerCase
  | TitleCase
  deriving (Eq, Show)

$(deriveLift ''Conversion)

-- | Generic format description. This is usable for integers, floats and strings.
data GenericFormat = GenericFormat {
    gfFillChar :: Char
  , gfAlign :: Maybe Align
  , gfSign :: Sign
  , gfLeading0x :: Bool
  , gfWidth :: Maybe Int
  , gfPrecision :: Maybe Int
  , gfRadix :: Maybe Radix
  , gfConvert :: Maybe Conversion
  }
  deriving (Eq, Show, Typeable)

$(deriveLift ''GenericFormat)

instance Default GenericFormat where
  def = GenericFormat {
          gfFillChar = ' '
        , gfAlign = Nothing
        , gfSign = OnlyNegative
        , gfLeading0x = False
        , gfWidth = Nothing
        , gfPrecision = Nothing
        , gfRadix = Nothing
        , gfConvert = Nothing
        }

data BoolFormat = BoolFormat {
    bfTrue :: TL.Text
  , bfFalse :: TL.Text
  }
  deriving (Eq, Show, Typeable)

$(deriveLift ''BoolFormat)

instance Default BoolFormat where
  def = BoolFormat "true" "false"

data MaybeFormat f = MaybeFormat TL.Text f
  deriving (Eq, Show, Typeable)

instance Default f => Default (MaybeFormat f) where
  def = MaybeFormat "" def

$(deriveLift ''MaybeFormat)

class GenericFormatType t where

----------------------- IsVarFormat instances --------------------------------------

instance IsVarFormat GenericFormat where
  type PresentableAs GenericFormat t = GenericFormatType t

instance IsVarFormat BoolFormat where
  type PresentableAs BoolFormat t = t ~ Bool

instance IsVarFormat f => IsVarFormat (MaybeFormat f) where
  type PresentableAs (MaybeFormat f) t = PresentableAs f t

