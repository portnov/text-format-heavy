
module Data.Text.Format.Heavy.Parse.Types
  (-- * Utility types
   Parser, ParserState (..), initParserState
  ) where

import qualified Data.Text.Lazy as TL
import Text.Parsec

data ParserState = ParserState {
    psNextIndex :: Int
  }
  deriving (Eq, Show)

initParserState :: ParserState
initParserState = ParserState 0

type Parser a = Parsec TL.Text ParserState a

