{-# LANGUAGE OverloadedStrings #-}
-- | This module defines shell-like syntax of format strings, generally described as
-- "any part after dollar sign is a variable substitution".
--
-- Examples of valid variable substitutions are:
--
-- * @"Simple: ${}"@. Note that to have auto-numbered placeholders in this syntax, you have to 
--   write @${}@; both dollar sign and braces are necessary.
--
-- * @"Numbered: $1"@ or @"Numbered: ${1}"@.
--
-- * @"Named: $var"@ or @"Named: ${var}"@.
--
-- * @"Specifying variable formatting: ${var:+8.4}"@. To specify variable format, you have to
--   use braces.
--
-- This syntax is not the default, so to use it you have to explicitly call @parseShellFormat'@:
--
-- @
-- {-\# LANGUAGE OverloadedStrings #\-}
-- module Main where
--
-- import Data.Time
-- import qualified Data.Text.Lazy.IO as TLIO
-- import Data.Text.Format.Heavy
-- import Data.Text.Format.Heavy.Parse.Shell
--
-- main :: IO ()
-- main = do
--   name <- getLine
--   time <- getZonedTime
--   TLIO.putStrLn $ format (parseShellFormat' "Hello, ${}! It is ${:%H:%M:%S} now.") (name, time)
-- @
--
module Data.Text.Format.Heavy.Parse.Shell
  (-- * Parse functions
   parseShellFormat, parseShellFormat',
   -- * Parsec functions
   pShellFormat
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
  noneOf "$" <|> try ('$' <$ string "$$")

pVerbatim :: Parser FormatItem
pVerbatim = (FString . TL.pack) `fmap` many1 anyChar'

pVariable :: Parser FormatItem
pVariable = do
    char '$'
    (name, fmt) <- try bracedVariable <|> unbracedVariable
    return $ FVariable (TL.pack name) fmt
  where
    bracedVariable = between (char '{') (char '}') $ do
      name <- many $ try alphaNum <|> try (char '-') <|> char '.'
      mbColon <- optionMaybe $ char ':'
      fmt <- case mbColon of
               Nothing -> return Nothing
               Just _ -> do
                  fmtStr <- many (noneOf "}" <|> try ('}' <$ string "\\}"))
                  return $ Just $ TL.pack fmtStr
      name' <- if null name
                 then do
                      st <- getState
                      let n = psNextIndex st
                      modifyState $ \st -> st {psNextIndex = psNextIndex st + 1}
                      return $ show n
                 else return name
      return (name', fmt)

    unbracedVariable = do
      name <- many1 alphaNum
      return (name, Nothing)

-- | Parsec parser for string format.
pShellFormat :: Parser Format
pShellFormat = Format `fmap` many (try pVariable <|> pVerbatim)

-- | Parse string format definition.
parseShellFormat :: TL.Text -> Either ParseError Format
parseShellFormat text = runParser pShellFormat initParserState "<format string>" text

-- | Version of parseShellFormat which throws @error@ in case of syntax error in the formatting string.
parseShellFormat' :: TL.Text -> Format
parseShellFormat' text = either (error . show) id $ parseShellFormat text

