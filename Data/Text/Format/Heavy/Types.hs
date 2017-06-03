{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
-- | This module contains basic type definitions
module Data.Text.Format.Heavy.Types where

import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

-- | Variable name
type VarName = TL.Text

-- | Variable format in text form. Nothing means default format.
type VarFormat = Maybe TL.Text

-- | String format item.
data FormatItem =
    FString TL.Text        -- ^ Verbatim text
  | FVariable {
      vName :: VarName      -- ^ Variable name
    , vFormat :: VarFormat  -- ^ Variable format
    }
  deriving (Show)

-- | String format
data Format = Format [FormatItem]
  deriving (Show)

-- | Can be used for different data types describing formats of specific types.
class (Default f, Show f) => IsVarFormat f where
  parseVarFormat :: TL.Text -> Either String f

instance IsVarFormat () where
  parseVarFormat "" = Right ()
  parseVarFormat fmt = Left $ "Unsupported format: " ++ TL.unpack fmt

-- | Value that can be formatted to be substituted into format string.
class Formatable a where
  format :: VarFormat -> a -> B.Builder

-- | Any variable that can be substituted.
data Variable = forall a. Formatable a => Variable a

instance Formatable Variable where
  format fmt (Variable x) = format fmt x

-- | Format one variable according to format specification.
formatVar :: VarFormat -> Variable -> B.Builder
formatVar fmt (Variable v) = format fmt v

-- | Data structure that contains some number of variables.
class VarContainer c where
  lookupVar :: VarName -> c -> Maybe Variable

------------------------------------------------------------------------------

-- | Alignment of string within specified width
data Align = AlignLeft | AlignRight | AlignCenter
  deriving (Eq, Show)

-- | Whether to show the sign of number
data Sign = Always | OnlyNegative | SpaceForPositive
  deriving (Eq, Show)

-- | Number base
data Radix = Decimal | Hexadecimal
  deriving (Eq, Show)

-- | Generic format description. This is usable for integers, floats and strings.
data GenericFormat = GenericFormat {
    gfFillChar :: Char
  , gfAlign :: Maybe Align
  , gfSign :: Sign
  , gfLeading0x :: Bool
  , gfWidth :: Maybe Int
  , gfPrecision :: Maybe Int
  , gfRadix :: Maybe Radix
  }
  deriving (Eq, Show)

instance Default GenericFormat where
  def = GenericFormat {
          gfFillChar = ' '
        , gfAlign = Nothing
        , gfSign = OnlyNegative
        , gfLeading0x = False
        , gfWidth = Nothing
        , gfPrecision = Nothing
        , gfRadix = Nothing
        }

