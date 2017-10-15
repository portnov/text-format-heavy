{-# LANGUAGE OverloadedStrings #-}
-- | This module contains parsers for formatting strings.
-- We have to deal with two kinds of strings:
--
-- * String formats. This is the whole construct like @"Hello, {}! Your account balance is {1:+8.4}."@.
-- * Variable formats. This is only part after colon in braces, i.e. the @+8.4@ thing in previous example.
--
-- The string format syntax is supposed to be very stable and simple.
-- There are more than one commonly used formatting string syntax, though. This package provides the
-- following syntaxes:
--
-- * The default syntax, which is basically defined by phrase "any part in braces is variable substitution".
--   This syntax is defined in @Data.Text.Format.Heavy.Parse.Braces@ module.
--
-- * Shell-like syntax, which is basically defined by phrase "any part starting with dollar sign is variable
--   substitution". This syntax is defined in @Data.Text.Format.Heavy.Parse.Shell@ module.
--
-- It is possible to define your own syntaxes: you just need to parse an instance of @Format@ type from some
-- sort of string. The default syntax will still remain default, in sence that @instance IsString Format@ is
-- defined in terms of this syntax in @Data.Text.Format.Heavy.Instances@ module.
--
-- Variable formats syntax depends on type of data which we are going to format. These formats can be
-- pretty complex, for example they can include alignment, rounding, and so on.
--
module Data.Text.Format.Heavy.Parse
  (-- * Parse functions
   parseFormat, parseFormat',
   parseGenericFormat, parseBoolFormat,
   parseMaybeFormat,
   -- * Parsec functions
   pGenericFormat, pBoolFormat
  ) where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats
import Data.Text.Format.Heavy.Parse.VarFormat
import Data.Text.Format.Heavy.Parse.Braces

