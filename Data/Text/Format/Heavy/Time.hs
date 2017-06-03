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
genericTimeFormat :: FormatTime t => VarFormat -> t -> B.Builder
genericTimeFormat Nothing x = B.fromString $ formatTime defaultTimeLocale rfc822DateFormat x
genericTimeFormat (Just fmtStr) x =
  B.fromString $ formatTime defaultTimeLocale (TL.unpack fmtStr) x

------------------------ Formatable instances -------------------------------------------

-- instance Formatable UniversalTime where
--   format fmt x = genericTimeFormat fmt x

instance Formatable Day where
  format fmt x = genericTimeFormat fmt x

instance Formatable UTCTime where
  format fmt x = genericTimeFormat fmt x

instance Formatable TimeZone where
  format fmt x = genericTimeFormat fmt x

instance Formatable TimeOfDay where
  format fmt x = genericTimeFormat fmt x

instance Formatable LocalTime where
  format fmt x = genericTimeFormat fmt x

instance Formatable ZonedTime where
  format fmt x = genericTimeFormat fmt x

