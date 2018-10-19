{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This module contains Formatable instances for time/date values,
-- which use Data.Time.Format notation for formats (like @%H:%M@).
-- Default date/time format is RFC 822.
--
-- This module is not re-exported by Data.Text.Format.Heavy by default,
-- because it defines only one of possible time formatting strings syntaxes.
-- One may like other syntax for some reason; if we re-exported this module by
-- default, it would be impossible to hide these instances to implement other.
--
module Data.Text.Format.Heavy.Time where

import Data.String
import Data.Char
import Data.Default
import Data.Time
import Data.Time.Format
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Language.Haskell.TH.Lift
import Instances.TH.Lift

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Parse
import Data.Text.Format.Heavy.Build

data WithTime p = WithTime p

instance FormatParser p => FormatParser (WithTime p) where
  parseStringFormat (WithTime p) = parseStringFormat p
  parseVarFormat (WithTime p) text = 
    case parseVarFormat p text of
      Right result -> Right result
      Left _ -> Right $ mkVarFormat $ TimeFormat $ TL.unpack text

newtype TimeFormat = TimeFormat String
  deriving (Eq, Show, Typeable)

instance Default TimeFormat where
  def = TimeFormat rfc822DateFormat

$(deriveLift ''TimeFormat)

instance IsVarFormat TimeFormat where
  type PresentableAs TimeFormat t = FormatTime t

-- | Generic time formatter, using Data.Time.Format
genericTimeFormat :: FormatTime t => VarFormat -> t -> Either String B.Builder
genericTimeFormat fmt x =
  case fromVarFormat fmt of
    Nothing -> Right $ B.fromString $ formatTime defaultTimeLocale rfc822DateFormat x
    Just (TimeFormat str) ->
        Right $ B.fromString $ formatTime defaultTimeLocale str x

------------------------ Formatable instances -------------------------------------------

-- instance Formatable UniversalTime where
--   formatVar fmt x = genericTimeFormat fmt x

instance Formatable Day where
  formatVar fmt x = genericTimeFormat fmt x

instance Formatable UTCTime where
  formatVar fmt x = genericTimeFormat fmt x

instance Formatable TimeZone where
  formatVar fmt x = genericTimeFormat fmt x

instance Formatable TimeOfDay where
  formatVar fmt x = genericTimeFormat fmt x

instance Formatable LocalTime where
  formatVar fmt x = genericTimeFormat fmt x

instance Formatable ZonedTime where
  formatVar fmt x = genericTimeFormat fmt x

