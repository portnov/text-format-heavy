{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This module contains basic type definitions
module Data.Text.Format.Heavy.Types where

import Data.Default
import Data.Monoid
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Lift
import Text.Parsec (ParseError)
import GHC.Exts (Constraint)

-- | Variable name
type VarName = TL.Text

-- | Variable format in text form. Nothing means default format.
data VarFormat =
    DefaultVarFormat
  | forall f. IsVarFormat f => AnyVarFormat f

instance Eq VarFormat where
  DefaultVarFormat == DefaultVarFormat = True
  (AnyVarFormat f1) == (AnyVarFormat f2) =
      typeOf f1 == typeOf f2 && show f1 == show f2
  _ == _ = False

instance Show VarFormat where
  show DefaultVarFormat = "default"
  show (AnyVarFormat f) = show f

fromVarFormat :: forall f. Typeable f => VarFormat -> Maybe f
fromVarFormat DefaultVarFormat = Nothing
fromVarFormat (AnyVarFormat f) = cast f

mkVarFormat :: IsVarFormat f => f -> VarFormat
mkVarFormat f = AnyVarFormat f

-- | String format item.
data FormatItem =
    FString TL.Text        -- ^ Verbatim text
  | FVariable {
      vName :: VarName      -- ^ Variable name
    , vFormat :: VarFormat  -- ^ Variable format
    }

instance Eq FormatItem where
  (FString t1) == (FString t2) = t1 == t2
  (FVariable n1 f1) == (FVariable n2 f2) =
      n1 == n2 && f1 == f2
  _ == _ = False

instance Show FormatItem where
  show (FString text) = TL.unpack text
  show (FVariable {..}) = TL.unpack $ "{" <> vName <> sFormat <> "}"
    where
      sFormat = case vFormat of
                  DefaultVarFormat -> ""
                  AnyVarFormat fmt -> ":" <> (TL.pack $ show fmt)

-- | String format
data Format = Format [FormatItem]
  deriving (Eq)

instance Show Format where
  show (Format lst) = concatMap show lst

instance Monoid Format where
  mempty = Format []
  mappend (Format xs) (Format ys) = Format (xs ++ ys)

-- | Can be used for different data types describing formats of specific types.
class (Default f, Show f, Typeable f, Lift f) => IsVarFormat f where
  type PresentableAs f t :: Constraint

  formatVar' :: (PresentableAs f t, Formatable t) => f -> t -> Either String B.Builder
  formatVar' fmt x = formatVar (AnyVarFormat fmt) x
--   -- | Left for errors.
--   parseVarFormat :: TL.Text -> Either String f

-- instance IsVarFormat () where
--   parseVarFormat "" = Right ()
--   parseVarFormat fmt = Left $ "Unsupported format: " ++ TL.unpack fmt

class FormatParser p where
  parseStringFormat :: p -> TL.Text -> Either ParseError Format
  parseVarFormat :: p -> TL.Text -> Either ParseError VarFormat

-- class Formatable' fmt a where
--   formatBy :: fmt -> a -> Either String B.Builder

-- | Value that can be formatted to be substituted into format string.
class Formatable a where
  -- | Format variable according to format specification.
  -- This function should usually parse format specification by itself.
  formatVar :: VarFormat                -- ^ Variable format specification in text form. Nothing is for default format.
            -> a                        -- ^ Variable value.
            -> Either String B.Builder  -- ^ Left for errors in variable format syntax, or errors during formatting.

-- | Any variable that can be substituted.
-- This type may be also used to construct heterogeneous lists:
-- @[Variable 1, Variable "x"] :: [Variable]@.
data Variable = forall a. Formatable a => Variable a

-- data Variable' fmt = forall a. Formatable' fmt a => Variable' a

instance Show Variable where
  show (Variable v) = either error toString $ formatVar DefaultVarFormat v
    where
      toString :: B.Builder -> String
      toString b = TL.unpack $ B.toLazyText b

instance Formatable Variable where
  formatVar fmt (Variable x) = formatVar fmt x

-- | Format one variable according to format specification.
formatAnyVar :: VarFormat -> Variable -> Either String B.Builder
formatAnyVar fmt (Variable v) = formatVar fmt v

-- | Data structure that contains some number of variables.
class VarContainer c where
  lookupVar :: VarName -> c -> Maybe Variable

class VarContainer c => ClosedVarContainer c where
  allVarNames :: c -> [VarName]

------------------------------------------------------------------------------

