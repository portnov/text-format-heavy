{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Format.Heavy.Parse where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

import Data.Text.Format.Heavy.Types

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

pFormat :: Parser Format
pFormat = Format `fmap` many (try pVariable <|> pVerbatim)

parseFormat :: TL.Text -> Either ParseError Format
parseFormat text = runParser pFormat initParserState "<format string>" text

parseFormat' :: TL.Text -> Format
parseFormat' text = either (error . show) id $ parseFormat text

pGenericFormat :: Parser GenericFormat
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
    pAlign :: Parser Align
    pAlign = do
      alignChar <- oneOf "<>"
      align <- case alignChar of
                 '<' -> return AlignLeft
                 '>' -> return AlignRight
                 _ -> fail $ "Unexpected align char: " ++ [alignChar]
      return align

    pAlignWithFill :: Parser (Char, Align)
    pAlignWithFill = do
      fill <- noneOf "<>=^"
      align <- pAlign
      return (fill, align)

    pAlignWithoutFill :: Parser (Char, Align)
    pAlignWithoutFill = do
      align <- pAlign
      return (' ', align)

    pFillAlign :: Parser (Char, Align)
    pFillAlign = do
      try pAlignWithoutFill <|> pAlignWithFill

    pSign :: Parser Sign
    pSign = do
      signChar <- oneOf "+- "
      sign <- case signChar of
                '+' -> return Always
                '-' -> return OnlyNegative
                ' ' -> return SpaceForPositive
                _ -> fail $ "Unexpected sign char: " ++ [signChar]
      return sign

    pLeading0x :: Parser Bool
    pLeading0x = do
      mbSharp <- optionMaybe $ char '#'
      case mbSharp of
        Nothing -> return False
        Just _ -> return True

    natural :: Parser Int
    natural = do
      ws <- many1 $ oneOf "0123456789"
      return $ read ws

    pWidth :: Parser Int
    pWidth = natural

    pPrecision :: Parser Int
    pPrecision = do
      char '.'
      natural
    
    pRadix :: Parser Radix
    pRadix = do
      rc <- oneOf "xhd"
      case rc of
        'x' -> return Hexadecimal
        'h' -> return Hexadecimal
        'd' -> return Decimal

parseGenericFormat :: TL.Text -> Either ParseError GenericFormat
parseGenericFormat text = runParser pGenericFormat initParserState "<variable format specification>" text

