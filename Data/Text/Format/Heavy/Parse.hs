{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Format.Heavy.Parse where

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

type Parser a = Parsec T.Text ParserState a

-- TODO: proper handling of escaping
anyChar' :: Parser Char
anyChar' =
  noneOf "{}" <|> try ('{' <$ string "\\{") <|> try ('}' <$ string "\\}")

pVerbatim :: Parser FormatItem
pVerbatim = (FString . T.pack) `fmap` many1 anyChar'

pVariable :: Parser FormatItem
pVariable = do
    (name, fmt) <- between (char '{') (char '}') variable
    return $ FVariable (T.pack name) fmt
  where
    variable = do
      name <- many alphaNum
      mbColon <- optionMaybe $ char ':'
      fmt <- case mbColon of
               Nothing -> return Nothing
               Just _ -> do
                  fmtStr <- many anyChar'
                  return $ Just $ T.pack fmtStr
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

parseFormat :: T.Text -> Either ParseError Format
parseFormat text = runParser pFormat initParserState "<format string>" text

parseFormat' :: T.Text -> Format
parseFormat' text = either (error . show) id $ parseFormat text

