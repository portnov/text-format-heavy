{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

module Data.Text.Format.Heavy.Instances where

import Data.String
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Parse

instance IsString Format where
  fromString str = parseFormat' (fromString str)

------------------------ Formatable instances -------------------------------------------

instance Formatable Int where
  format Nothing x = decimal x
  format (Just "x") x = hexadecimal x
  format (Just "h") x = hexadecimal x
  format (Just fmt) _ = error $ "Unknown integer format: " ++ TL.unpack fmt

instance Formatable Integer where
  format Nothing x = decimal x
  format (Just "x") x = hexadecimal x
  format (Just "h") x = hexadecimal x
  format (Just fmt) _ = error $ "Unknown integer format: " ++ TL.unpack fmt

instance Formatable String where
  format _ str = B.fromText $ T.pack str

instance Formatable T.Text where
  format _ text = B.fromText text

instance Formatable TL.Text where
  format _ text = B.fromLazyText text

data Single a = Single {getSingle :: a}
  deriving (Eq, Show)

-- data Many a = Many {getMany :: [a]}
--   deriving (Eq, Show)

instance Formatable a => Formatable (Single a) where
  format fmt (Single x) = format fmt x

data Shown a = Shown { shown :: a }
  deriving (Eq)

instance Show a => Show (Shown a) where
  show (Shown x) = show x

instance Show a => Formatable (Shown a) where
  format _ (Shown x) = B.fromLazyText $ TL.pack $ show x

instance Formatable a => Formatable (Maybe a) where
  format Nothing Nothing = mempty
  format (Just "") Nothing = mempty
  format fmt (Just x) = format fmt x

instance (Formatable a, Formatable b) => Formatable (Either a b) where
  format fmt (Left x) = format fmt x
  format fmt (Right y) = format fmt y

------------------------------- VarContainer instances -------------------------------------

instance Formatable a => VarContainer (Single a) where
  lookupVar "0" (Single x) = Just $ Variable x
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b) => VarContainer (a, b) where
  lookupVar "0" (a,_) = Just $ Variable a
  lookupVar "1" (_,b) = Just $ Variable b
  lookupVar _ _ = Nothing
  
instance (Formatable a, Formatable b, Formatable c) => VarContainer (a, b, c) where
  lookupVar "0" (a,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_) = Just $ Variable b
  lookupVar "2" (_,_,c) = Just $ Variable c
  lookupVar _ _ = Nothing
  
instance (Formatable a, Formatable b, Formatable c, Formatable d) => VarContainer (a, b, c, d) where
  lookupVar "0" (a,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d) = Just $ Variable d
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e)
     => VarContainer (a, b, c, d, e) where
  lookupVar "0" (a,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e) = Just $ Variable e
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f)
     => VarContainer (a, b, c, d, e, f) where
  lookupVar "0" (a,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f) = Just $ Variable f
  lookupVar _ _ = Nothing

instance Formatable a => VarContainer [a] where
  lookupVar name lst =
    if not $ TL.all isDigit name
      then Nothing
      else let n = read (TL.unpack name)
           in  if n >= length lst
               then Nothing
               else Just $ Variable (lst !! n)
  
instance Formatable x => VarContainer [(TL.Text, x)] where
  lookupVar name pairs = Variable `fmap` lookup name pairs

instance Formatable x => VarContainer (M.Map TL.Text x) where
  lookupVar name pairs = Variable `fmap` M.lookup name pairs

