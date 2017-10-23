{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Format.Heavy.Parse.VarFormat
  where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats

-- | Parsec parser for generic (Python-like) variable format.
pGenericFormat :: Parsec TL.Text st GenericFormat
pGenericFormat = do
    mbFillAlign <- optionMaybe (try pFillAlign <?> "fill and align specification")
    let fill = fromMaybe ' ' $ fst `fmap` mbFillAlign
    let align = snd `fmap` mbFillAlign
    mbSign <- optionMaybe (pSign <?> "sign specification")
    let sign = fromMaybe OnlyNegative mbSign
    mbLeading0x <- optionMaybe (pLeading0x <?> "leading 0x specification")
    let leading0x = fromMaybe False mbLeading0x
    mbWidth <- optionMaybe (pWidth <?> "width specification")
    mbPrecision <- optionMaybe (pPrecision <?> "precision specification")
    mbRadix <- optionMaybe (pRadix <?> "radix specification")
    mbConvert <- optionMaybe (pConvert <?> "conversion specification")
    return $ GenericFormat {
               gfFillChar = fill
             , gfAlign = align
             , gfSign = sign
             , gfLeading0x = leading0x
             , gfWidth = mbWidth
             , gfPrecision = mbPrecision
             , gfRadix = mbRadix
             , gfConvert = mbConvert
             }
  where
    pAlign :: Parsec TL.Text st Align
    pAlign = do
      alignChar <- oneOf "<>^"
      align <- case alignChar of
                 '<' -> return AlignLeft
                 '>' -> return AlignRight
                 '^' -> return AlignCenter
                 _ -> fail $ "Unexpected align char: " ++ [alignChar]
      return align

    pAlignWithFill :: Parsec TL.Text st (Char, Align)
    pAlignWithFill = do
      fill <- noneOf "<>=^"
      align <- pAlign
      return (fill, align)

    pAlignWithoutFill :: Parsec TL.Text st (Char, Align)
    pAlignWithoutFill = do
      align <- pAlign
      return (' ', align)

    pFillAlign :: Parsec TL.Text st (Char, Align)
    pFillAlign = do
      try pAlignWithoutFill <|> pAlignWithFill

    pSign :: Parsec TL.Text st Sign
    pSign = do
      signChar <- oneOf "+- "
      sign <- case signChar of
                '+' -> return Always
                '-' -> return OnlyNegative
                ' ' -> return SpaceForPositive
                _ -> fail $ "Unexpected sign char: " ++ [signChar]
      return sign

    pLeading0x :: Parsec TL.Text st Bool
    pLeading0x = do
      mbSharp <- optionMaybe $ char '#'
      case mbSharp of
        Nothing -> return False
        Just _ -> return True

    natural :: Parsec TL.Text st Int
    natural = do
      ws <- many1 $ oneOf "0123456789"
      return $ read ws

    pWidth :: Parsec TL.Text st Int
    pWidth = natural

    pPrecision :: Parsec TL.Text st Int
    pPrecision = do
      char '.'
      natural
    
    pRadix :: Parsec TL.Text st Radix
    pRadix = do
      rc <- oneOf "xhd"
      case rc of
        'x' -> return Hexadecimal
        'h' -> return Hexadecimal
        'd' -> return Decimal

    pConvert :: Parsec TL.Text st Conversion
    pConvert = do
      char '~'
      conv <- oneOf "ult"
      case conv of
        'u' -> return UpperCase
        'l' -> return LowerCase
        't' -> return TitleCase

-- | Parse generic variable format.
--
-- Syntax is:
--
-- @
-- [[fill]align][sign][#][width][.precision][radix][~conversion]
-- @
--
-- where:
--
-- * fill - padding character (space by default)
-- * align - alignment indicator (@<@, @>@, or @^@)
-- * sign - when to show number's sign (@+@, @-@, or space)
-- * @#@ - if specified, then for hexadecimal numbers the leading @0x@ will be added
-- * width - minimum length of the field
-- * precision - number of decimal places after point, for floatting-point numbers
-- * radix - @h@ or @x@ for hexadecimal, @d@ for decimal (default).
-- * conversion - text conversion symbol. Supported are: @u@ - convert to upper case,
--   @l@ - convert to lower case, @t@ - convert to title case (capitalize all words).
--
parseGenericFormat :: TL.Text -> Either ParseError GenericFormat
parseGenericFormat text = runParser pGenericFormat () "<variable format specification>" text

-- | Parsec parser for Bool format
pBoolFormat :: Parsec TL.Text st BoolFormat
pBoolFormat = do
  true <- many $ noneOf ":,;"
  oneOf ":,;"
  false <- many $ anyChar
  return $ BoolFormat (TL.pack true) (TL.pack false)

-- | Parse Bool format.
--
-- Syntax is:
--
-- @
-- TRUE:FALSE
-- @
--
-- Colon can be replaced with comma or semicolon.
--
-- For example, valid format specifications are @true:false@ (the default one),
-- @True:False@, @yes:no@, and so on.
--
parseBoolFormat :: TL.Text -> Either ParseError BoolFormat
parseBoolFormat text = runParser pBoolFormat () "<boolean format specification>" text

-- | Try to parse format for @Maybe x@ type.
-- The syntax is:
--
-- @
-- someformat|nothing
-- @
--
-- where @someformat@ is format for the @x@ type, and @nothing@ is the string
-- to be substituted for @Nothing@ value.
--
-- Returns Nothing, if format does not contain @|@. Otherwise, returns
-- @Just (someformat, nothing)@.
--
parseMaybeFormat :: TL.Text -> Maybe (TL.Text, TL.Text)
parseMaybeFormat text =
  let (xFmtStr, nothingStr) = TL.breakOnEnd "|" text
  in  if TL.null xFmtStr
        then Nothing
        else Just (TL.init xFmtStr, nothingStr)

