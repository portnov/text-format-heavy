{-# LANGUAGE ExistentialQuantification #-}

module Data.Text.Format.Heavy.Types where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

type VarName = T.Text
type VarFormat = Maybe T.Text

data FormatItem =
    FString T.Text
  | FVariable {
      vName :: VarName
    , vFormat :: VarFormat
    }
  deriving (Eq, Show)

data Format = Format [FormatItem]
  deriving (Eq, Show)

class Formatable a where
  format :: VarFormat -> a -> B.Builder

data Variable = forall a. Formatable a => Variable a

instance Formatable Variable where
  format fmt (Variable x) = format fmt x

formatVar :: VarFormat -> Variable -> B.Builder
formatVar fmt (Variable v) = format fmt v

class VarContainer c where
  lookupVar :: VarName -> c -> Maybe Variable

