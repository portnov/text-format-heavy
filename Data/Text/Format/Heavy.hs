-- | This is the main module of @text-format-heavy@ library.
-- 
-- In most cases, you need to import only this module, and probably also the 
-- Data.Text.Format.Heavy.Time module, if you want to format time/date values.
-- 
-- This package exports the @format@ function and @Format@ data type. 
-- The Format type implements the instance of IsString, so in the code you may
-- use formatting strings as literals, if you enable @OverloadedStrings@ extension.
--
-- Formatting strings syntax is based on Python's string.format() syntax.
--
-- The simple usage example is
--
-- @
-- {-\# LANGUAGE OverloadedStrings #\-}
-- module Main where
--
-- import Data.Time
-- import qualified Data.Text.Lazy.IO as TLIO
-- import Data.Text.Format.Heavy
-- import Data.Text.Format.Heavy.Time
--
-- main :: IO ()
-- main = do
--   name <- getLine
--   time <- getZonedTime
--   TLIO.putStrLn $ format "Hello, {}! It is {:%H:%M:%S} now." (name, time)
-- @
--    
module Data.Text.Format.Heavy
  (
    module Data.Text.Format.Heavy.Types,
    module Data.Text.Format.Heavy.Build,
    module Data.Text.Format.Heavy.Instances
  ) where

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Build (format)
import Data.Text.Format.Heavy.Instances (Single (..), Shown (..))

