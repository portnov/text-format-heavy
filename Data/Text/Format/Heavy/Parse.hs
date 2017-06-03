{-# LANGUAGE OverloadedStrings #-}
-- | This module contains parsers for formatting strings.
-- We have to deal with two kinds of strings:
--
-- * String formats. This is the whole construct like @"Hello, {}! Your account balance is {1:+8.4}."@.
-- * Variable formats. This is only part after colon in braces, i.e. the @+8.4@ thing in previous example.
--
-- The string format syntax is supposed to be very stable and simple. It is basically defined by phrase
-- "any part in braces is variable substitution".
--
-- Variable formats syntax depends on type of data which we are going to format. These formats can be
-- pretty complex, for example they can include alignment, rounding, and so on.
--
module Data.Text.Format.Heavy.Parse
  (-- * Parse functions
   parseFormat, parseFormat',
   parseGenericFormat,
   -- * Parsec functions
   pFormat, pGenericFormat,
   -- * Utility types
   Parser, ParserState (..), initParserState
  ) where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats

data ParserState = ParserState {
    psNextIndex :: Int
  }
  deriving (Eq, Show)

initParserState :: ParserState
initParserState = ParserState 0

type Parser a = Parsec TL.Text ParserState a

-- TODO: proper handling of escaping
anyChar' :: Parser Char
anyChar' =
  noneOf "{}" <|> try ('{' <$ string "\\{") <|> try ('}' <$ string "\\}")

pVerbatim :: Parser FormatItem
pVerbatim = (FString . TL.pack) `fmap` many1 anyChar'

pVariable :: Parser FormatItem
pVariable = do
    (name, fmt) <- between (char '{') (char '}') variable
    return $ FVariable (TL.pack name) fmt
  where
    variable = do
      name <- many alphaNum
      mbColon <- optionMaybe $ char ':'
      fmt <- case mbColon of
               Nothing -> return Nothing
               Just _ -> do
                  fmtStr <- many anyChar'
                  return $ Just $ TL.pack fmtStr
      name' <- if null name
                 then do
                      st <- getState
                      let n = psNextIndex st
                      modifyState $ \st -> st {psNextIndex = psNextIndex st + 1}
                      return $ show n
                 else return name
      return (name', fmt)

-- | Parsec parser for string format.
pFormat :: Parser Format
pFormat = Format `fmap` many (try pVariable <|> pVerbatim)

-- | Parse string format definition.
parseFormat :: TL.Text -> Either ParseError Format
parseFormat text = runParser pFormat initParserState "<format string>" text

-- | Version of parseFormat which throws @error@ in case of syntax error in the formatting string.
parseFormat' :: TL.Text -> Format
parseFormat' text = either (error . show) id $ parseFormat text

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
    return $ GenericFormat {
               gfFillChar = fill
             , gfAlign = align
             , gfSign = sign
             , gfLeading0x = leading0x
             , gfWidth = mbWidth
             , gfPrecision = mbPrecision
             , gfRadix = mbRadix
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

-- | Parse generic variable format.
parseGenericFormat :: TL.Text -> Either ParseError GenericFormat
parseGenericFormat text = runParser pGenericFormat () "<variable format specification>" text

