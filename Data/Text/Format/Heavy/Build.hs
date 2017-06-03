
module Data.Text.Format.Heavy.Build where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

import Data.Text.Format.Heavy.Types

makeBuilder :: VarContainer c => Format -> c -> B.Builder
makeBuilder (Format items) vars = mconcat $ map go items
  where
    go (FString s) = B.fromLazyText s
    go (FVariable name fmt) =
      case lookupVar name vars of
        Nothing -> error $ "Parameter not found: " ++ TL.unpack name
        Just var -> formatVar fmt var

formatText :: VarContainer vars => Format -> vars -> TL.Text
formatText fmt vars = B.toLazyText $ makeBuilder fmt vars

