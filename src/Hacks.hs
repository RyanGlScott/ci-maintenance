{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hacks where

import           Repos

import           Data.Bifunctor (first)
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (GenericPackageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Text (display)

data Hack
  = HLint [String]
  deriving (Eq, Ord, Read, Show)

addHLintCPPDefine :: Hack -> String -> Hack
addHLintCPPDefine (HLint oldDefs) newDef = HLint (newDef:oldDefs)
-- addHLintCPPDefine h               _      = h

applyHacks :: RepoMetadata -> String -> Maybe String
applyHacks (RM repo _ comps) travisYmlContents =
  fmap doHacks $ Map.lookup repo hacksMap
  where
    doHacks :: [Hack] -> String
    doHacks = foldl' (flip doHack) travisYmlContents

    doHack :: Hack -> String -> String
    doHack (HLint cppDefines) = hlintHack cppDefines comps

hlintHack :: [String] -> [Component]
          -> String -> String
hlintHack cppDefines componentNames =
  unlines . part3 . part2 . part1 . lines
  where
    part1, part2, part3 :: [String] -> [String]

    part1 ls =
      let (prior, unsetLine:rest) = break ("  - unset CC" `isPrefixOf`) ls
      in    prior
         ++ [ unsetLine
            , "  - export HLINTVER=2.0.9"
            , "  - mkdir ~/.hlint"
            , "  - curl -L https://github.com/ndmitchell/hlint/releases/download/v$HLINTVER/hlint-$HLINTVER-x86_64-linux.tar.gz | tar -xz --strip-components=1 -C ~/.hlint"
            ]
         ++ rest

    part2 ls =
      let (prior, pathLine:rest) = break ("  - \"PATH=" `isPrefixOf`) ls
          ' ':' ':'-':' ':'"':'P':'A':'T':'H':'=':pathRest = pathLine
      in    prior
         ++ ["  - \"PATH=~/.hlint:" ++ pathRest]
         ++ rest

    part3 ls =
      let (prior, rest) = break ("# REGENDATA [" `isPrefixOf`) ls
      in    prior
         ++ "  # hlint"
          : map
              (\(Component{compName, compGpd}) ->
                   "  - (cd "
                ++ compName
                ++ "-* && hlint "
                ++ unwords (sourceDirs compGpd)
                ++ " --cpp-ansi"
                ++ concatMap (\cppDef -> " --cpp-define=" ++ cppDef) cppDefines
                ++ ")")
              componentNames
         ++ ""
          : rest

-- Cargo-culted from haskell-ci's @doctestArgs@.
sourceDirs :: GenericPackageDescription -> [String]
sourceDirs gpd = case PD.library $ flattenPackageDescription gpd of
    Nothing -> []
    Just l  -> dirsOrMods
      where
        bi = PD.libBuildInfo l

        dirsOrMods
            | null (PD.hsSourceDirs bi) = map display (PD.exposedModules l)
            | otherwise = PD.hsSourceDirs bi

hacksMap :: Map Repo [Hack]
hacksMap = Map.fromList $ concat
  [ map (first (Repo "ekmett"))
    [ ("bits",          [hlint])
    , ("contravariant", [hlint])
    , ("folds",         [hlint])
    , ("gc",            [hlint])
    , ("heaps",         [hlint])
    , ("hyphenation",   [hlint])
    , ("ersatz",        [hlint])
    , ("lens",          [hlint])
    , ("log-domain",    [hlint `addHLintCPPDefine` "__USE_FFI__"])
    , ("profunctors",   [hlint])
    , ("structs",       [hlint])
    , ("rcu",           [hlint])
    , ("zippers",       [hlint])
    ]
    -- Miscellaneous
  , [ (Repo "bos" "criterion", [])
    , (Repo "goldfirere" "singletons", [])
    , (Repo "haskell" "primitive", [])
    , (Repo "ku-fpg" "blank-canvas", [])
    ]
  ]
  where
    hlint :: Hack
    hlint = HLint ["HLINT"]
