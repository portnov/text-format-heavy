{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Time
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Lazy (Text)

import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Time

main :: IO ()
main = hspec $ do
  describe "trivial" $ do
    it "formats string literal without formatting characters" $ do
      format "hello world" () `shouldBe` "hello world"

  describe "simple" $ do
    it "formats int properly" $ do
      format "integer: {}" (Single (7 :: Int)) `shouldBe` "integer: 7"

    it "formats strings properly" $ do
      format "string: {}" (Single ("hello" :: String)) `shouldBe` "string: hello"

    it "handles parameter numbers" $ do
      format "one: {0}, two: {1}" ((1:: Int), (2::Int)) `shouldBe` "one: 1, two: 2"
      format "two: {1}, one: {0}" ((1:: Int), (2::Int)) `shouldBe` "two: 2, one: 1"

    describe "handles parameters names" $ do
      it "with ascii characters" $ do
        format "one: {theKey}!"
          ((Map.singleton "theKey" "the string") :: Map Text Text)
            `shouldBe` "one: the string!"
      it "with dots" $ do
        format "one: {the.key}!"
          ((Map.singleton "the.key" "the string") :: Map Text Text)
            `shouldBe` "one: the string!"
      it "with dashes" $ do
        format "one: {the-key}!"
          ((Map.singleton "the-key" "the string") :: Map Text Text)
            `shouldBe` "one: the string!"
      it "with underscores" $ do
        format "one: {the_key}!"
          ((Map.singleton "the_key" "the string") :: Map Text Text)
            `shouldBe` "one: the string!"

  describe "documentation" $ do
    it "formats examples from wiki" $ do
      format "hex: {:#x}" (Single (427 :: Int)) `shouldBe` "hex: 0x1ab"
      format "hex: {:#h}" (Single (427 :: Int)) `shouldBe` "hex: 0x1ab"
      format "hex: {:#X}" (Single (427 :: Int)) `shouldBe` "hex: 0x1AB"
      format "hex: {:#H}" (Single (427 :: Int)) `shouldBe` "hex: 0x1AB"
      format "dec: {:#d}" (Single (17 :: Int)) `shouldBe` "dec: 17"
      format "center: <{0:^10}>" (Single ("hello" :: String)) `shouldBe` "center: <   hello  >"
      format "float: {:+6.4}" (Single (2.718281828 :: Double)) `shouldBe` "float: +2.7183"

    it "formats booleans" $ do
      format "default: {}" (Single True) `shouldBe` "default: true"
      format "enable: {:yes:no}" (Single False) `shouldBe` "enable: no"

    it "formats maybes" $ do
      format "Value: {:.3|<undefined>}." (Single (2.718281828 :: Float)) `shouldBe` "Value: 2.718."
      format "Value: {:.3|<undefined>}." (Single (Nothing :: Maybe Float)) `shouldBe` "Value: <undefined>."
      format "Value: {:.3}." (Single (Nothing :: Maybe Float)) `shouldBe` "Value: ."

    it "formats time" $ do
      let yektLocale = defaultTimeLocale
                         { knownTimeZones = [TimeZone (5 * 60) False "YEKT"] }
            -- `defaultTimeLocale` does not know about Yekaterinburg.
          Just time =  parseTimeM True yektLocale rfc822DateFormat "Sat,  3 Jun 2017 19:06:01 YEKT" :: Maybe ZonedTime
      format "time: {:%H:%M:%S}" (Single time) `shouldBe` "time: 19:06:01"
      format "time: {:%H:%M:%S %Z}" (Single time) `shouldBe` "time: 19:06:01 YEKT"
      format "default: {}" (Single time) `shouldBe` "default: Sat,  3 Jun 2017 19:06:01 YEKT"

