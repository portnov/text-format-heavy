{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, OverloadedStrings #-}

module Data.Text.Format.Heavy.Types where

import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

type VarName = TL.Text

data DefinedVarFormat = forall f. IsVarFormat f => DefinedVarFormat f

instance Show DefinedVarFormat where 
  show (DefinedVarFormat x) = show x

type VarFormat = Maybe TL.Text

data FormatItem =
    FString TL.Text
  | FVariable {
      vName :: VarName
    , vFormat :: VarFormat
    }
  deriving (Show)

data Format = Format [FormatItem]
  deriving (Show)

class (Default f, Show f) => IsVarFormat f where
  parseVarFormat :: TL.Text -> Either String f

instance IsVarFormat () where
  parseVarFormat "" = Right ()
  parseVarFormat fmt = Left $ "Unsupported format: " ++ TL.unpack fmt

class Formatable a where
  format :: VarFormat -> a -> B.Builder

data Variable = forall a. Formatable a => Variable a

instance Formatable Variable where
  format fmt (Variable x) = format fmt x

formatVar :: VarFormat -> Variable -> B.Builder
formatVar fmt (Variable v) = format fmt v

class VarContainer c where
  lookupVar :: VarName -> c -> Maybe Variable

data Align = AlignLeft | AlignRight
  deriving (Eq, Show)

data Sign = Always | OnlyNegative | SpaceForPositive
  deriving (Eq, Show)

data Radix = Decimal | Hexadecimal
  deriving (Eq, Show)

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

