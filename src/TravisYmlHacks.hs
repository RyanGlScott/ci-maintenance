{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module TravisYmlHacks (applyTravisYmlHacks) where

import           Repos

import           Data.Bifunctor (first)
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (GenericPackageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Text (display)

applyTravisYmlHacks :: RepoMetadata -> String -> Maybe String
applyTravisYmlHacks (RM repo _proj _projContents comps) travisYmlContents =
  fmap doHacks $ Map.lookup repo hacksMap
  where
    doHacks :: [TravisYmlHack] -> String
    doHacks = foldl' (flip doHack) travisYmlContents

    doHack :: TravisYmlHack -> String -> String
    doHack (HLint cppDefines)                   = hlintHack cppDefines comps
    doHack DisableTestsGlobally                 = disableTestsGloballyHack
    doHack (AlternateConfig cabalFlags)         = alternateConfigHack cabalFlags

data TravisYmlHack
  = HLint [String]
  | DisableTestsGlobally
  | AlternateConfig [String]
  deriving (Eq, Ord, Read, Show)

addHLintCPPDefine :: TravisYmlHack -> String -> TravisYmlHack
addHLintCPPDefine (HLint oldDefs) newDef = HLint (newDef:oldDefs)
addHLintCPPDefine h               _      = h

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

-- Cargo-culted from haskell-ci's @doctestArgs@.
sourceDirs :: GenericPackageDescription -> [String]
sourceDirs gpd = case PD.library $ flattenPackageDescription gpd of
    Nothing -> ["."] -- ¯\_(ツ)_/¯
    Just l  -> dirsOrMods
      where
        bi = PD.libBuildInfo l

        dirsOrMods
            | null (PD.hsSourceDirs bi) = map display (PD.exposedModules l)
            | otherwise = PD.hsSourceDirs bi

hacksMap :: Map Repo [TravisYmlHack]
hacksMap = Map.fromList $ concat
  [ map (first (mkRepo "ekmett"))
    [ ("bits",          [hlint])
    , ("compensated",   [hlint])
    , ("contravariant", [hlint])
    , ("folds",         [hlint])
    , ("gc",            [hlint])
    , ("heaps",         [hlint])
    , ("hyphenation",   [hlint, AlternateConfig ["-fembed"]])
    , ("ersatz",        [hlint])
    , ("lens",          [hlint])
    , ("log-domain",    [hlint `addHLintCPPDefine` "__USE_FFI__"])
    , ("profunctors",   [hlint])
    , ("structs",       [hlint])
    , ("rcu",           [hlint, AlternateConfig ["-funstable"]])
    , ("zippers",       [hlint])
    ]
  , [ (mkRepo "lens" "lens-aeson", [hlint]) ]

  , map (first (mkRepo "ku-fpg"))
    [ ("blank-canvas",      [DisableTestsGlobally])
    , ("javascript-bridge", [DisableTestsGlobally])
    ]
  , [ (Repo "ku-fpg" "blank-canvas" (OtherBranch "0.6"), [DisableTestsGlobally]) ]

    -- Miscellaneous
  , [ (mkRepo "bos" "criterion", [AlternateConfig ["-fembed-data-files"]])
    ]
  ]
  where
    hlint :: TravisYmlHack
    hlint = HLint ["HLINT"]
