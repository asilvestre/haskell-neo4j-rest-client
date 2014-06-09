{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import Control.Monad
--import Data.Array
--import Data.Binary
--import Data.Generics.Zipper
import Data.Int
import Data.List
import Data.Maybe
--import Data.NBT
import Distribution.Simple.Utils
import Distribution.Verbosity
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck
import qualified Data.ByteString.Lazy as B
import qualified Test.HUnit as H


-- Test Main
-- ============================================================================
main :: IO ()
main = $(defaultMainGenerator)
