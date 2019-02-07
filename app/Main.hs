{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Main where

import           Lib     ()
-- import           Lib       (Foo (..))

-- import           Conv (toS)
import           Prelude

class StringConv a b where
  strConv :: a -> b

toS :: StringConv a b => a -> b
toS = strConv


main :: IO ()
main = do
  loop "blah"

  where
    loop foo = do
      pure $ Foo (toS foo)
      pure ()
