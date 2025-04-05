{-# LANGUAGE OverloadedStrings #-}
-- | This module defines the default syntax of format strings, generally described as
-- "any part in braces is variable substitution".
--
-- Examples of valid variable substitutions are:
--
-- * @"Simple: {}"@
--
-- * @"Numbered: {0}"@
--
-- * @"Named: {var}"@
--
-- * @"Specifying variable formatting: {var:+8.4}"@
--
module Data.Text.Format.Heavy.Parse.Braces
  (-- * Parse functions
   parseFormat, parseFormat',
   -- * Parsec functions
   pBracesFormat
     ) where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Parsec

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats
import Data.Text.Format.Heavy.Parse.Types

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
      name <- many $ try alphaNum <|> try (char '-') <|> char '.' <|> char '_'
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
pBracesFormat :: Parser Format
pBracesFormat = Format `fmap` many (try pVariable <|> pVerbatim)

-- | Parse string format definition.
parseFormat :: TL.Text -> Either ParseError Format
parseFormat text = runParser pBracesFormat initParserState "<format string>" text

-- | Version of parseFormat which throws @error@ in case of syntax error in the formatting string.
parseFormat' :: TL.Text -> Format
parseFormat' text = either (error . show) id $ parseFormat text

