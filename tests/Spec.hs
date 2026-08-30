{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Build (formatEither)
import Data.Text.Format.Heavy.Parse (FormatParseItem (..), parse)
import Data.Text.Format.Heavy.Parse.Shell
import Data.Text.Format.Heavy.Time

main :: IO ()
main = hspec $ do
  describe "trivial" $ do
    it "formats string literal without formatting characters" $ do
      format "hello world" () `shouldBe` "hello world"

  describe "parse" $ do
    it "returns replacement fields" $ do
      parse "{name}"
        `shouldBe` Right [FormatReplacementField "name" Nothing]
      parse "{name:.2}"
        `shouldBe` Right [FormatReplacementField "name" (Just ".2")]

    it "keeps strings and fields in order" $ do
      parse "{{name}} {name}"
        `shouldBe` Right [FormatString "{name} ", FormatReplacementField "name" Nothing]

    it "keeps nested format specs raw" $ do
      parse "{value:{width}}"
        `shouldBe` Right [FormatReplacementField "value" (Just "{width}")]

  describe "simple" $ do
    it "formats int properly" $ do
      format "integer: {}" (Single (7 :: Int)) `shouldBe` "integer: 7"

    it "formats strings properly" $ do
      format "string: {}" (Single ("hello" :: String)) `shouldBe` "string: hello"

    it "formats text values properly" $ do
      format "strict: {}" (Single (T.pack "hello")) `shouldBe` "strict: hello"
      format "lazy: {}" (Single (TL.pack "hello")) `shouldBe` "lazy: hello"

    it "formats utf8 byte strings properly" $ do
      format "strict: {}" (Single (BS.pack [104, 101, 108, 108, 111])) `shouldBe` "strict: hello"
      format "lazy: {}" (Single (BSL.pack [104, 101, 108, 108, 111])) `shouldBe` "lazy: hello"

    it "handles parameter numbers" $ do
      format "one: {0}, two: {1}" ((1:: Int), (2::Int)) `shouldBe` "one: 1, two: 2"
      format "two: {1}, one: {0}" ((1:: Int), (2::Int)) `shouldBe` "two: 2, one: 1"

    describe "handles parameters names" $ do
      it "with ascii characters" $ do
        format "one: {theKey}!"
          ((Map.singleton "theKey" "the string") :: Map TL.Text TL.Text)
            `shouldBe` "one: the string!"
      it "with dots" $ do
        format "one: {the.key}!"
          ((Map.singleton "the.key" "the string") :: Map TL.Text TL.Text)
            `shouldBe` "one: the string!"
      it "with dashes" $ do
        format "one: {the-key}!"
          ((Map.singleton "the-key" "the string") :: Map TL.Text TL.Text)
            `shouldBe` "one: the string!"
      it "with underscores" $ do
        format "one: {the_key}!"
          ((Map.singleton "the_key" "the string") :: Map TL.Text TL.Text)
            `shouldBe` "one: the string!"

    it "handles additional variable containers" $ do
      format "{0}, {1}, {2}" (("one" :: String), (2 :: Int), ("three" :: String))
        `shouldBe` "one, 2, three"
      format "{0}, {2}" (Several ["zero", "one", "two" :: String])
        `shouldBe` "zero, two"
      format "hello {name}" ([ ("name", "world" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "hello world"

    it "handles defaulting containers" $ do
      format "present: {0}; missing: {1}" (Single ("value" :: String) `withDefault` Variable ("fallback" :: TL.Text))
        `shouldBe` "present: value; missing: fallback"
      format "present: {0}; missing: {1}" (optional (Single ("value" :: String)))
        `shouldBe` "present: value; missing: "

    it "handles escaped braces" $ do
      format "{{}}" () `shouldBe` "{}"
      format "\\{name\\}" () `shouldBe` "{name}"
      format "{{{name}}}" ([ ("name", "world" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "{world}"
      format "json: {{\"answer\": \"{answer}\"}}" ([ ("answer", "ok" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "json: {\"answer\": \"ok\"}"

  describe "documentation" $ do
    it "formats examples from wiki" $ do
      format "hex: {:#x}" (Single (427 :: Int)) `shouldBe` "hex: 0x1ab"
      format "hex: {:#h}" (Single (427 :: Int)) `shouldBe` "hex: 0x1ab"
      format "hex: {:#X}" (Single (427 :: Int)) `shouldBe` "hex: 0x1AB"
      format "hex: {:#H}" (Single (427 :: Int)) `shouldBe` "hex: 0x1AB"
      format "dec: {:#d}" (Single (17 :: Int)) `shouldBe` "dec: 17"
      format "center: <{0:^10}>" (Single ("hello" :: String)) `shouldBe` "center: <   hello  >"
      format "float: {:+6.4}" (Single (2.718281828 :: Double)) `shouldBe` "float: +2.7183"

    it "formats alignment and text conversion options" $ do
      format "left: <{:<8}>" (Single ("hi" :: String)) `shouldBe` "left: <hi      >"
      format "right: <{:>8}>" (Single ("hi" :: String)) `shouldBe` "right: <      hi>"
      format "center: <{:^7}>" (Single ("hi" :: String)) `shouldBe` "center: <   hi  >"
      format "fill: <{:*>8}>" (Single ("hi" :: String)) `shouldBe` "fill: <******hi>"
      format "upper: {:~u}" (Single ("hello" :: String)) `shouldBe` "upper: HELLO"
      format "lower: {:~l}" (Single ("HELLO" :: String)) `shouldBe` "lower: hello"
      format "title: {:~t}" (Single ("hello world" :: String)) `shouldBe` "title: Hello World"

    it "formats numeric options" $ do
      format "positive: {:+d}" (Single (7 :: Int)) `shouldBe` "positive: +7"
      format "negative: {:+d}" (Single (-7 :: Int)) `shouldBe` "negative: -7"
      format "space: {: d}" (Single (7 :: Int)) `shouldBe` "space:  7"
      format "hex: {:#X}" (Single (255 :: Integer)) `shouldBe` "hex: 0xFF"

    it "formats booleans" $ do
      format "default: {}" (Single True) `shouldBe` "default: true"
      format "enable: {:yes:no}" (Single False) `shouldBe` "enable: no"

    it "formats maybes" $ do
      format "Value: {:.3|<undefined>}." (Single (2.718281828 :: Float)) `shouldBe` "Value: 2.718."
      format "Value: {:.3|<undefined>}." (Single (Nothing :: Maybe Float)) `shouldBe` "Value: <undefined>."
      format "Value: {:.3}." (Single (Nothing :: Maybe Float)) `shouldBe` "Value: ."

    it "formats either and shown values" $ do
      format "left: {}" (Single (Left ("error" :: String) :: Either String Int)) `shouldBe` "left: error"
      format "right: {}" (Single (Right (7 :: Int) :: Either String Int)) `shouldBe` "right: 7"
      format "shown: {}" (Single (Shown (True, False))) `shouldBe` "shown: (True,False)"

    it "formats time" $ do
      let yektLocale = defaultTimeLocale
                         { knownTimeZones = [TimeZone (5 * 60) False "YEKT"] }
            -- `defaultTimeLocale` does not know about Yekaterinburg.
          Just time =  parseTimeM True yektLocale rfc822DateFormat "Sat,  3 Jun 2017 19:06:01 YEKT" :: Maybe ZonedTime
      format "time: {:%H:%M:%S}" (Single time) `shouldBe` "time: 19:06:01"
      format "time: {:%H:%M:%S %Z}" (Single time) `shouldBe` "time: 19:06:01 YEKT"
      format "default: {}" (Single time) `shouldBe` "default: Sat,  3 Jun 2017 19:06:01 YEKT"

  describe "errors" $ do
    it "reports missing parameters" $ do
      formatEither "missing: {1}" (Single ("value" :: String))
        `shouldBe` Left "Parameter not found: 1"
      formatEither "missing: {2}" (Several ["zero", "one" :: String])
        `shouldBe` Left "Parameter not found: 2"

  describe "shell syntax" $ do
    it "formats shell-style substitutions" $ do
      format (parseShellFormat' "hello $name") ([ ("name", "world" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "hello world"
      format (parseShellFormat' "hello ${name}") ([ ("name", "world" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "hello world"
      format (parseShellFormat' "${} ${}") (("one" :: String), ("two" :: String))
        `shouldBe` "one two"
      format (parseShellFormat' "cost: $$${amount}") ([ ("amount", "10" :: TL.Text) ] :: [(TL.Text, TL.Text)])
        `shouldBe` "cost: $10"
