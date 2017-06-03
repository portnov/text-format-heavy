{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
-- | This module contains Formatable instances for time/date values,
-- which use Data.Time.Format notation for formats (like @%H:%M@).
module Data.Text.Format.Heavy.Time where

import Data.String
import Data.Char
import Data.Default
import Data.Time
import Data.Time.Format
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Parse
import Data.Text.Format.Heavy.Build

-- | Generic time formatter, using Data.Time.Format
genericTimeFormat :: FormatTime t => VarFormat -> t -> Either String B.Builder
genericTimeFormat Nothing x = Right $ B.fromString $ formatTime defaultTimeLocale rfc822DateFormat x
genericTimeFormat (Just fmtStr) x =
  Right $ B.fromString $ formatTime defaultTimeLocale (TL.unpack fmtStr) x

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

