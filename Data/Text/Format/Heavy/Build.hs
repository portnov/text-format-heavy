{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Format.Heavy.Build
  (format, formatEither,
   makeBuilder,
   -- * Formatters building utilities
   align, applySign, applySharp,
   formatInt, formatStr, formatFloat, formatBool
  ) where

import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats

makeBuilder :: VarContainer c => Format -> c -> Either String B.Builder
makeBuilder (Format items) vars = mconcat `fmap` mapM go items
  where
    go (FString s) = Right $ B.fromLazyText s
    go (FVariable name fmt) =
      case lookupVar name vars of
        Nothing -> Left $ "Parameter not found: " ++ TL.unpack name
        Just var -> formatVar fmt var

-- | The main formatting function.
-- This function throws @error@ if some error detected during format string parsing or formatting itself.
format :: VarContainer vars => Format -> vars -> TL.Text
format fmt vars = either error id $ formatEither fmt vars

-- | The main formatting function.
-- This version returns @Left@ value with error description in case of error in 
-- format string or error during formatting.
formatEither :: VarContainer vars => Format -> vars -> Either String TL.Text
formatEither fmt vars = either Left (Right . B.toLazyText) $ makeBuilder fmt vars

align' :: Int -> Align -> Char -> B.Builder -> B.Builder
align' width AlignLeft fill text =
  B.fromLazyText $ TL.justifyLeft (fromIntegral width) fill $ B.toLazyText text
align' width AlignRight fill text =
  B.fromLazyText $ TL.justifyRight (fromIntegral width) fill $ B.toLazyText text
align' width AlignCenter fill text =
  B.fromLazyText $ TL.center (fromIntegral width) fill $ B.toLazyText text

-- | Align text within available width according to format
align :: GenericFormat -> B.Builder -> B.Builder
align fmt text =
  case (gfAlign fmt, gfWidth fmt) of
    (Just a, Just w) -> align' w a (gfFillChar fmt) text
    _ -> text

-- | Add @+/-@ sign to the number representation, if required
applySign :: (Num a, Ord a) => Sign -> a -> B.Builder -> B.Builder
applySign Always x text =
  if x >= 0
    then B.singleton '+' <> text
    else B.singleton '-' <> text
applySign OnlyNegative x text =
  if x >= 0
    then text
    else B.singleton '-' <> text
applySign SpaceForPositive x text =
  if x >= 0
    then B.singleton ' ' <> text
    else B.singleton '-' <> text

-- | Add @0x@ to the number representation, if required
applySharp :: Bool -> Radix -> B.Builder -> B.Builder
applySharp False _ text = text
applySharp True Decimal text = text
applySharp True Hexadecimal text = B.fromLazyText "0x" <> text

-- | Format integer number according to GenericFormat
formatInt :: Integral a => GenericFormat -> a -> B.Builder
formatInt fmt x = align fmt $ applySign (gfSign fmt) x $ applySharp (gfLeading0x fmt) radix $ inRadix
  where
   radix = fromMaybe Decimal (gfRadix fmt)
   inRadix = case radix of
               Decimal -> decimal (abs x)
               Hexadecimal -> hexadecimal (abs x)

-- | Format floating-point number according to GenericFormat
formatFloat :: RealFloat a => GenericFormat -> a -> B.Builder
formatFloat fmt x =
  align fmt $ applySign (gfSign fmt) x $ formatRealFloat Fixed (gfPrecision fmt) $ abs x

-- | Format Text according to GenericFormat.
formatStr :: GenericFormat -> TL.Text -> B.Builder
formatStr fmt text = align fmt $ B.fromLazyText text

-- | Format boolean value.
formatBool :: BoolFormat -> Bool -> B.Builder
formatBool fmt True = B.fromLazyText $ bfTrue fmt
formatBool fmt False = B.fromLazyText $ bfFalse fmt

