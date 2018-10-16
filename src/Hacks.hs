{-# LANGUAGE OverloadedStrings #-}
module Hacks where

import           Repos

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

data Hack
  = HLint

applyHacks :: Repo -> String -> Maybe String
applyHacks repo travisYmlContents =
  fmap go $ Map.lookup repo hacksMap
  where
    go :: [Hack] -> String
    go _ = travisYmlContents

hacksMap :: Map Repo [Hack]
hacksMap = Map.fromList
  [ (Repo "bos" "criterion", [])
  , (Repo "goldfirere" "singletons", [])
  , (Repo "haskell" "primitive", [])
  , (Repo "ku-fpg" "blank-canvas", [])
  ]
