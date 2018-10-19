{-# LANGUAGE OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Text.Format.Heavy.TH
  (makeQuoter,
   format,
   shell
  )
  where

import qualified Data.Text.Lazy as TL
import Data.Maybe
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Instances.TH.Lift
import Labels

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Parse.Braces as Braces
import Data.Text.Format.Heavy.Parse.Shell as Shell
import qualified Data.Text.Format.Heavy.Build as Build (format)
import Data.Text.Format.Heavy.Formats
import Data.Text.Format.Heavy.Parse.VarFormat

$(deriveLift ''VarFormat)
$(deriveLift ''FormatItem)
$(deriveLift ''Format)

makeQuoter :: (TL.Text -> Either ParseError Format) -> QuasiQuoter
makeQuoter parser = QuasiQuoter go failQ failQ failQ
  where
    failQ _ = fail "this quasiquoter works in expression context only"

    go str = case parser (TL.pack str) of
               Left err -> fail $ show err
               Right fmt -> makeFunction fmt

makeFunction :: Format -> Q Exp
makeFunction fmt@(Format items) = do
    pat <- mkRecordPat varTypes
    body <- [| Build.format fmt (convertDict $(varE $ mkName "record") ) |]
    return $ LamE [pat] body
  where
    varTypes :: [(TL.Text, Maybe TH.Type)]
    varTypes = mapMaybe getType items

    getType :: FormatItem -> Maybe (TL.Text, Maybe TH.Type)
    getType (FString _) = Nothing
    getType (FVariable name fmt) = Just (name, undefined)

    mkHasCtx :: [(TL.Text, Maybe TH.Type)] -> Q TH.Cxt
    mkHasCtx pairs = mapM mkHasPred pairs

    mkHasPred :: (TL.Text, Maybe TH.Type) -> Q TH.Pred
    mkHasPred (name, mbType) = do
      let valType = case mbType of
                      Nothing -> VarT $ mkName $ TL.unpack name
                      Just t -> t
      let record = return $ VarT $ mkName "record"
      [t|Has $(return $ LitT $ StrTyLit $ TL.unpack name) $(return valType) $(record) |]

    mkRecordType :: [(TL.Text, Maybe TH.Type)] -> Q TH.Type
    mkRecordType pairs = do
      ctx <- mkHasCtx pairs
      let record = mkName "record"
      return $ ForallT [PlainTV record] ctx (VarT record)

    mkRecordPat :: [(TL.Text, Maybe TH.Type)] -> Q TH.Pat
    mkRecordPat pairs = do
      recType <- mkRecordType pairs
      let record = mkName "record"
      return $ SigP (VarP record) recType


format :: QuasiQuoter
format = makeQuoter Braces.parseFormat

shell :: QuasiQuoter
shell = makeQuoter Shell.parseShellFormat

-- detectType :: VarFormat -> Q (Maybe TH.Type)
-- detectType Nothing = return Nothing
-- detectType (VarFormat text) =

convertDict :: Reflect Formatable r => r -> [(TL.Text, Variable)]
convertDict r = map (\(k,v) -> (TL.pack k, v)) $ reflect @Formatable Variable r
--   where
--     go :: forall v. Formatable v => Variable
--     go v = Variable v

-- let fn = \(r :: ((Has "name" name r, Has "surname" surname r) => r))  -> Data.Text.Format.Heavy.format "Hello, {name} {surname}" (convertDict r)
