{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Conv (
  StringConv (..)
, toS
, Leniency (..)
) where

import           Data.ByteString.Char8    as B
import           Data.Text                as T
import           Data.Text.Encoding       as T
import           Data.Text.Encoding.Error as T

import           Control.Applicative      (pure)
import           Data.Eq                  (Eq (..))
import           Data.Function            (id, (.))
import           Data.Ord                 (Ord (..))
import           Data.String              (String)
import           Protolude.Base

data Leniency = Lenient | Strict
  deriving (Eq,Show,Ord,Enum,Bounded)

class StringConv a b where
  strConv :: Leniency -> a -> b

toS :: StringConv a b => a -> b
toS = strConv Strict

instance StringConv String String where strConv _ = id
instance StringConv String B.ByteString where strConv _ = B.pack
instance StringConv String T.Text where strConv _ = T.pack

instance StringConv B.ByteString String where strConv _ = B.unpack
instance StringConv B.ByteString B.ByteString where strConv _ = id
instance StringConv B.ByteString T.Text where strConv = decodeUtf8T

instance StringConv T.Text String where strConv _ = T.unpack
instance StringConv T.Text B.ByteString where strConv _ = T.encodeUtf8
instance StringConv T.Text T.Text where strConv _ = id


decodeUtf8T :: Leniency -> B.ByteString -> T.Text
decodeUtf8T Lenient = T.decodeUtf8With T.lenientDecode
decodeUtf8T Strict  = T.decodeUtf8With T.strictDecode
