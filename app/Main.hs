{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Lib       ()
-- import           Lib       (Foo (..))
import           Protolude

main :: IO ()
main = do
  loop "blah"

  where
    loop foo = do
      pure $ Foo (toS foo)
      pure ()
