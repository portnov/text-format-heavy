{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
-- | This module contains Formatable and VarContainer instances for most used types.
module Data.Text.Format.Heavy.Instances
  (-- * Utility data types
   Single (..), Several (..), Shown (..),
   -- * Combinators
   DefaultValue (..), ThenCheck (..), WithDefault,
   withDefault, optional,
   -- * Generic formatters
   genericIntFormat, genericFloatFormat
  ) where

import Data.String
import Data.Char
import Data.Default
import Data.Word
import Data.Int
import Data.Maybe
import Data.List (union)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

import Data.Text.Format.Heavy.Types
import Data.Text.Format.Heavy.Formats
import Data.Text.Format.Heavy.Parse
import Data.Text.Format.Heavy.Build

instance IsString Format where
  fromString str = parseFormat' (fromString str)

---------------------- Generic formatters -------------------------------------------

-- | Generic formatter for integer types
genericIntFormat :: Integral a => VarFormat -> a -> Either String B.Builder
genericIntFormat fmt x =
  Right $ formatInt (fromMaybe def $ fromVarFormat fmt) x

-- | Generic formatter for floating-point types
genericFloatFormat :: RealFloat a => VarFormat -> a -> Either String B.Builder
genericFloatFormat fmt x =
  Right $ formatFloat (fromMaybe def $ fromVarFormat fmt) x

------------------------ Formatable instances -------------------------------------------

-- | Unit type is formatted as empty string
instance Formatable () where
  formatVar _ _ = Right mempty

instance GenericFormatType () where

instance Formatable Int where
  formatVar fmt x = genericIntFormat fmt x

instance GenericFormatType Int where

instance Formatable Int8 where
  formatVar fmt x = genericIntFormat fmt x

instance GenericFormatType Int8 where

instance Formatable Int16 where
  formatVar fmt x = genericIntFormat fmt x

instance GenericFormatType Int16 where

instance Formatable Int32 where
  formatVar fmt x = genericIntFormat fmt x

instance GenericFormatType Int32 where

instance Formatable Int64 where
  formatVar fmt x = genericIntFormat fmt x

instance GenericFormatType Int64 where

instance Formatable Word8 where
  formatVar fmt x = genericIntFormat fmt x

instance Formatable Word16 where
  formatVar fmt x = genericIntFormat fmt x

instance Formatable Word32 where
  formatVar fmt x = genericIntFormat fmt x

instance Formatable Word64 where
  formatVar fmt x = genericIntFormat fmt x

instance Formatable Integer where
  formatVar fmt x = genericIntFormat fmt x

instance Formatable Float where
  formatVar fmt x = genericFloatFormat fmt x

instance Formatable Double where
  formatVar fmt x = genericFloatFormat fmt x

instance Formatable String where
  formatVar fmt text =
    Right $ formatStr (fromMaybe def $ fromVarFormat fmt) (fromString text)

instance Formatable T.Text where
  formatVar fmt text =
    Right $ formatStr (fromMaybe def $ fromVarFormat fmt) (TL.fromStrict text)

instance Formatable TL.Text where
  formatVar fmt text =
    Right $ formatStr (fromMaybe def $ fromVarFormat fmt) text

instance Formatable BS.ByteString where
  formatVar fmt text =
    Right $ formatStr (fromMaybe def $ fromVarFormat fmt) $ TL.fromStrict $ TE.decodeUtf8 text

instance Formatable BSL.ByteString where
  formatVar fmt text =
    Right $ formatStr (fromMaybe def $ fromVarFormat fmt) $ TLE.decodeUtf8 text

instance Formatable Bool where
  formatVar fmt x =
    Right $ formatBool (fromMaybe def $ fromVarFormat fmt) x

-- | Container for single parameter.
-- Example usage:
--
-- @
-- format "Hello, {}!" (Single name)
-- @
data Single a = Single {getSingle :: a}
  deriving (Eq, Show)

instance Formatable a => Formatable (Single a) where
  formatVar fmt (Single x) = formatVar fmt x

-- | Container for several parameters of the same type.
-- Example usage:
--
-- @
-- format "{} + {} = {}" $ Several [2, 3, 5]
-- @
data Several a = Several {getSeveral :: [a]}
  deriving (Eq, Show)

-- | Values packed in Shown will be formatted using their Show instance.
--
-- For example,
--
-- @
-- formatText "values: {}." (Shown (True, False)) ==> "values: (True, False)."
-- @
data Shown a = Shown { shown :: a }
  deriving (Eq)

instance Show a => Show (Shown a) where
  show (Shown x) = show x

instance Show a => Formatable (Shown a) where
  formatVar _ (Shown x) = Right $ B.fromLazyText $ TL.pack $ show x

instance Formatable a => Formatable (Maybe a) where
  formatVar DefaultVarFormat Nothing = Right mempty
  formatVar DefaultVarFormat (Just x) = formatVar DefaultVarFormat x
  formatVar f m =
    case fromVarFormat f of
      Nothing -> case m of
                   Nothing -> Right mempty
                   Just x -> formatVar f x
      Just (MaybeFormat dflt fmt) ->
        case m of
          Nothing -> Right $ B.fromLazyText dflt
          Just x  -> formatVar fmt x

instance (Formatable a, Formatable b) => Formatable (Either a b) where
  formatVar fmt (Left x) = formatVar fmt x
  formatVar fmt (Right y) = formatVar fmt y

------------------------------- VarContainer instances -------------------------------------

instance Formatable a => VarContainer (Single a) where
  lookupVar "0" (Single x) = Just $ Variable x
  lookupVar _ _ = Nothing

instance Formatable a => ClosedVarContainer (Single a) where
  allVarNames _ = ["0"]

instance VarContainer () where
  lookupVar _ _ = Nothing

instance ClosedVarContainer () where
  allVarNames _ = []

-- | Maybe container contains one variable (named 0); Nothing contains an empty string.
instance Formatable a => VarContainer (Maybe a) where
  lookupVar "0" (Just x) = Just $ Variable x
  lookupVar "0" Nothing = Just $ Variable ()
  lookupVar _ _ = Nothing

instance Formatable a => ClosedVarContainer (Maybe a) where
  allVarNames Nothing = []
  allVarNames (Just _) = ["0"]

instance (Formatable a, Formatable b) => VarContainer (a, b) where
  lookupVar "0" (a,_) = Just $ Variable a
  lookupVar "1" (_,b) = Just $ Variable b
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b) => ClosedVarContainer (a, b) where
  allVarNames _ = ["0", "1"]
  
instance (Formatable a, Formatable b, Formatable c) => VarContainer (a, b, c) where
  lookupVar "0" (a,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_) = Just $ Variable b
  lookupVar "2" (_,_,c) = Just $ Variable c
  lookupVar _ _ = Nothing
  
instance (Formatable a, Formatable b, Formatable c) => ClosedVarContainer (a, b, c) where
  allVarNames _ = ["0", "1", "2"]

instance (Formatable a, Formatable b, Formatable c, Formatable d) => VarContainer (a, b, c, d) where
  lookupVar "0" (a,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d) = Just $ Variable d
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d) => ClosedVarContainer (a, b, c, d) where
  allVarNames _ = ["0", "1", "2", "3"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e)
     => VarContainer (a, b, c, d, e) where
  lookupVar "0" (a,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e) = Just $ Variable e
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e)
     => ClosedVarContainer (a, b, c, d, e) where
  allVarNames _ = ["0", "1", "2", "3", "4"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f)
     => VarContainer (a, b, c, d, e, f) where
  lookupVar "0" (a,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f) = Just $ Variable f
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f)
     => ClosedVarContainer (a, b, c, d, e, f) where
  allVarNames _ = ["0", "1", "2", "3", "4", "5"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g)
     => VarContainer (a, b, c, d, e, f, g) where
  lookupVar "0" (a,_,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f,_) = Just $ Variable f
  lookupVar "6" (_,_,_,_,_,_,g) = Just $ Variable g
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g)
     => ClosedVarContainer (a, b, c, d, e, f, g) where
  allVarNames _ = ["0", "1", "2", "3", "4", "5", "6"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h)
     => VarContainer (a, b, c, d, e, f, g, h) where
  lookupVar "0" (a,_,_,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_,_,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f,_,_) = Just $ Variable f
  lookupVar "6" (_,_,_,_,_,_,g,_) = Just $ Variable g
  lookupVar "7" (_,_,_,_,_,_,_,h) = Just $ Variable h
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h)
     => ClosedVarContainer (a, b, c, d, e, f, g, h) where
  allVarNames _ = ["0", "1", "2", "3", "4", "5", "6", "7"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h, Formatable i)
     => VarContainer (a, b, c, d, e, f, g, h, i) where
  lookupVar "0" (a,_,_,_,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_,_,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_,_,_,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f,_,_,_) = Just $ Variable f
  lookupVar "6" (_,_,_,_,_,_,g,_,_) = Just $ Variable g
  lookupVar "7" (_,_,_,_,_,_,_,h,_) = Just $ Variable h
  lookupVar "8" (_,_,_,_,_,_,_,_,i) = Just $ Variable i
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h, Formatable i)
     => ClosedVarContainer (a, b, c, d, e, f, g, h, i) where
  allVarNames _ = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h, Formatable i, Formatable j)
     => VarContainer (a, b, c, d, e, f, g, h, i, j) where
  lookupVar "0" (a,_,_,_,_,_,_,_,_,_) = Just $ Variable a
  lookupVar "1" (_,b,_,_,_,_,_,_,_,_) = Just $ Variable b
  lookupVar "2" (_,_,c,_,_,_,_,_,_,_) = Just $ Variable c
  lookupVar "3" (_,_,_,d,_,_,_,_,_,_) = Just $ Variable d
  lookupVar "4" (_,_,_,_,e,_,_,_,_,_) = Just $ Variable e
  lookupVar "5" (_,_,_,_,_,f,_,_,_,_) = Just $ Variable f
  lookupVar "6" (_,_,_,_,_,_,g,_,_,_) = Just $ Variable g
  lookupVar "7" (_,_,_,_,_,_,_,h,_,_) = Just $ Variable h
  lookupVar "8" (_,_,_,_,_,_,_,_,i,_) = Just $ Variable i
  lookupVar "9" (_,_,_,_,_,_,_,_,_,j) = Just $ Variable j
  lookupVar _ _ = Nothing

instance (Formatable a, Formatable b, Formatable c, Formatable d, Formatable e, Formatable f, Formatable g,
          Formatable h, Formatable i, Formatable j)
     => ClosedVarContainer (a, b, c, d, e, f, g, h, i, j) where
  allVarNames _ = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

instance Formatable a => VarContainer (Several a) where
  lookupVar name (Several lst) =
    if not $ TL.all isDigit name
      then Nothing
      else let n = read (TL.unpack name)
           in  if n >= length lst
               then Nothing
               else Just $ Variable (lst !! n)
  
instance Formatable a => ClosedVarContainer (Several a) where
  allVarNames (Several lst) = map (TL.pack . show) [0 .. length lst - 1]

instance Formatable x => VarContainer [(TL.Text, x)] where
  lookupVar name pairs = Variable `fmap` lookup name pairs

instance Formatable x => ClosedVarContainer [(TL.Text, x)] where
  allVarNames pairs = map fst pairs

instance Formatable x => VarContainer (M.Map TL.Text x) where
  lookupVar name pairs = Variable `fmap` M.lookup name pairs

instance Formatable x => ClosedVarContainer (M.Map TL.Text x) where
  allVarNames pairs = M.keys pairs

-- | Variable container which contains fixed value for any variable name.
data DefaultValue = DefaultValue Variable

instance VarContainer DefaultValue where
  lookupVar _ (DefaultValue var) = Just var

-- | Combiled variable container, which uses parameters from @c1@,
-- and if variable is not found there it will check in @c2@.
data ThenCheck c1 c2 = ThenCheck c1 c2

-- | Convenience type synonym.
type WithDefault c = ThenCheck c DefaultValue

instance (VarContainer c1, VarContainer c2) => VarContainer (ThenCheck c1 c2) where
  lookupVar name (ThenCheck c1 c2) =
    case lookupVar name c1 of
      Just result -> Just result
      Nothing -> lookupVar name c2

instance (ClosedVarContainer c1, ClosedVarContainer c2) => ClosedVarContainer (ThenCheck c1 c2) where
  allVarNames (ThenCheck c1 c2) =
    allVarNames c1 `union` allVarNames c2

-- | Use variables from specified container, or use default value if
-- variable is not found in container.
withDefault :: VarContainer c => c -> Variable -> WithDefault c
withDefault c value = c `ThenCheck` DefaultValue value

-- | Use variables from specified container, or use empty string
-- variable is not found in container.
optional :: VarContainer c => c -> WithDefault c
optional c = c `withDefault` (Variable TL.empty)

