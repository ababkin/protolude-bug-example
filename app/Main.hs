{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Main where

import           Lib       ()
-- import           Lib     (Foo (..))

import qualified Data.Text as T
import           Prelude

-- taken and simplified from the Protolude package:
-- https://github.com/sdiehl/protolude/blob/master/src/Protolude/Conv.hs
class StringConv a b where
  strConv :: a -> b

toS :: StringConv a b => a -> b
toS = strConv

instance StringConv T.Text String where strConv = T.unpack


main :: IO ()
main = do
  loop ("blah" :: T.Text)

  where
    loop foo =
      print $ Foo (toS foo)
