{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Format.Heavy.Build where

import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat

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

align' :: Int -> Align -> Char -> B.Builder -> B.Builder
align' width AlignLeft fill text =
  B.fromLazyText $ TL.justifyLeft (fromIntegral width) fill $ B.toLazyText text
align' width AlignRight fill text =
  B.fromLazyText $ TL.justifyRight (fromIntegral width) fill $ B.toLazyText text
align' width AlignCenter fill text =
  B.fromLazyText $ TL.center (fromIntegral width) fill $ B.toLazyText text

align :: GenericFormat -> B.Builder -> B.Builder
align fmt text =
  case (gfAlign fmt, gfWidth fmt) of
    (Just a, Just w) -> align' w a (gfFillChar fmt) text
    _ -> text

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

applySharp :: Bool -> Radix -> B.Builder -> B.Builder
applySharp False _ text = text
applySharp True Decimal text = text
applySharp True Hexadecimal text = B.fromLazyText "0x" <> text

formatInt :: Integral a => GenericFormat -> a -> B.Builder
formatInt fmt x = align fmt $ applySign (gfSign fmt) x $ applySharp (gfLeading0x fmt) radix $ inRadix
  where
   radix = fromMaybe Decimal (gfRadix fmt)
   inRadix = case radix of
               Decimal -> decimal (abs x)
               Hexadecimal -> hexadecimal (abs x)

formatFloat :: RealFloat a => GenericFormat -> a -> B.Builder
formatFloat fmt x =
  align fmt $ applySign (gfSign fmt) x $ formatRealFloat Fixed (gfPrecision fmt) $ abs x

formatStr :: GenericFormat -> TL.Text -> B.Builder
formatStr fmt text = align fmt $ B.fromLazyText text

