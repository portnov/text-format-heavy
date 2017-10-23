{-# LANGUAGE OverloadedStrings #-}

import Data.Time
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Time

main :: IO ()
main = do
  let template = "x: {:#x}, y: <{:^10}>, z: {:+6.4}, x(d): {0}, t: {}, y: {}, r: {}; bool: {:yes:no}."
      -- xs = [Variable (18 :: Int), Variable ("hello" :: String), Variable (3 :: Int)]
      rt = (Right 7) :: Either String Int
      xs = (18 :: Int, "hello" :: String, 2.718281828 :: Double, Shown (Just (7 :: Int)), Just (8 :: Int), rt, True)
  TLIO.putStrLn $ format template xs
  time <- getZonedTime
  let vars :: [(TL.Text, Variable)]
      vars = [("name", Variable ("Ilya" :: String)),
              ("time", Variable time),
              ("header.content-type", Variable ("text/json" :: String)),
              ("upper", Variable ("this will be uppercase" :: String)),
              ("lower", Variable ("This will be ALL lower case" :: String)),
              ("title", Variable ("this is the title" :: String)),
              ("noun", Variable ("string" :: String))]
  TLIO.putStrLn $
    format "Hello, {name}! It is {time:%H:%M:%S} now. Test {noun}ification. Content: {header.content-type} x. Upper: <{upper:~u}>, lower <{lower:~l}>, title: <{title:~t}>." vars
  let mbX = Nothing :: Maybe Float
      mbY = Just 7.37491 :: Maybe Float
      mbZ = Nothing :: Maybe Float
  TLIO.putStrLn $ format "Maybe X: {:+8.4|<not defined>}, Maybe Y: {:+8.4|<not defined>}, Maybe Z: {:+8.4}." (mbX, mbY, mbZ)
  

