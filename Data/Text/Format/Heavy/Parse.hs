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

