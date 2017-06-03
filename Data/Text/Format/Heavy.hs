-- | This is the main module of @text-format-heavy@ library.
-- 
-- In most cases, you need to import only this module, and probably also the 
-- Data.Text.Format.Heavy.Time module, if you want to format time/date values.
--
module Data.Text.Format.Heavy
  (
    module Data.Text.Format.Heavy.Types,
    module Data.Text.Format.Heavy.Build,
    module Data.Text.Format.Heavy.Instances
  ) where

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Build (formatText)
import Data.Text.Format.Heavy.Instances (Single (..), Shown (..))

