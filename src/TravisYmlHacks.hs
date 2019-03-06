{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module TravisYmlHacks (applyTravisYmlHacks) where

import           Repos

import           Data.Bifunctor (first)
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

applyTravisYmlHacks :: RepoMetadata -> String -> Maybe String
applyTravisYmlHacks (RM repo _proj) travisYmlContents =
  fmap doHacks $ Map.lookup repo hacksMap
  where
    doHacks :: [TravisYmlHack] -> String
    doHacks = foldl' (flip doHack) travisYmlContents

    doHack :: TravisYmlHack -> String -> String
    doHack DisableTestsGlobally         = disableTestsGloballyHack
    doHack (AlternateConfig cabalFlags) = alternateConfigHack cabalFlags

data TravisYmlHack
  = DisableTestsGlobally
  | AlternateConfig [String]
  deriving (Eq, Ord, Read, Show)

disableTestsGloballyHack :: String -> String
disableTestsGloballyHack = unlines . disableThemTests . lines
  where
    disableThemTests :: [String] -> [String]
    disableThemTests ls =
      let (prior, testLine:rest) = break ("  - if [ \"x$TEST\" = \"x--enable-tests\" ]; then ${CABAL} new-test" `isPrefixOf`) ls
          ' ':' ':testLineRest = testLine
      in    prior
         ++ ("  # " ++ testLineRest)
          : rest

alternateConfigHack :: [String] -> String -> String
alternateConfigHack cabalFlags = unlines . insertAlternateConfig . lines . useEnvVar
  where
    useEnvVar :: String -> String
    useEnvVar = replace "-w ${HC}" "-w ${HC} ${CABALFLAGS}"

    insertAlternateConfig :: [String] -> [String]
    insertAlternateConfig ls =
      let (prior, compiler:env:addons:rest) = break ("    - compiler: \"" `isPrefixOf`) ls
      in    prior
         ++ [ compiler
            , env
            , addons

            , compiler
            , "      env: CABALFLAGS=\"" ++ unwords cabalFlags ++ "\""
            , addons
            ]
         ++ rest

hacksMap :: Map Repo [TravisYmlHack]
hacksMap = Map.fromList $ concat
  [ map (first (mkRepo "ekmett"))
    [ ("hyphenation",   [AlternateConfig ["-fembed"]])
    , ("rcu",           [AlternateConfig ["-funstable"]])
    ]

  , map (first (mkRepo "ku-fpg"))
    [ ("blank-canvas",      [DisableTestsGlobally])
    , ("javascript-bridge", [DisableTestsGlobally])
    ]
  , [ (Repo "ku-fpg" "blank-canvas" (OtherBranch "0.6"), [DisableTestsGlobally]) ]

    -- Miscellaneous
  , [ (mkRepo "bos" "criterion", [AlternateConfig ["-fembed-data-files"]])
    ]
  ]
