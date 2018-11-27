{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hacks where

import           Repos

import           Data.Bifunctor (first)
import           Data.Char
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Distribution.PackageDescription as PD
import           Distribution.PackageDescription (GenericPackageDescription)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Text (display)

data Hack
  = HLint [String]
  | DisableTestsGlobally
  | AlternateConfig [String]
  | CabalProjectMiscellanea [String]
  deriving (Eq, Ord, Read, Show)

addHLintCPPDefine :: Hack -> String -> Hack
addHLintCPPDefine (HLint oldDefs) newDef = HLint (newDef:oldDefs)
addHLintCPPDefine h               _      = h

applyHacks :: RepoMetadata -> String -> Maybe String
applyHacks (RM repo _proj projContents comps) travisYmlContents =
  fmap doHacks $ Map.lookup repo hacksMap
  where
    doHacks :: [Hack] -> String
    doHacks = foldl' (flip doHack) travisYmlContents

    doHack :: Hack -> String -> String
    doHack (HLint cppDefines)                   = hlintHack cppDefines comps
    doHack DisableTestsGlobally                 = disableTestsGloballyHack
    doHack (AlternateConfig cabalFlags)         = alternateConfigHack cabalFlags
    doHack (CabalProjectMiscellanea extraLines) = cabalProjectMiscellaneaHack projContents extraLines

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
      let (prior, testLine:rest) = break ("  - if [ \"x$TEST\" = \"x--enable-tests\" ]; then cabal new-test" `isPrefixOf`) ls
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

cabalProjectMiscellaneaHack :: String -> [String]
                            -> String -> String
cabalProjectMiscellaneaHack projContents extraLines =
  unlines . go . lines
  where
    go :: [String] -> [String]
    go ls =
      let (prior, rest) = break ("  - \"printf 'packages: " `isPrefixOf`) ls
      in if null rest
            then ls
            else let (printfLine:rest') = rest
                 in    prior
                    ++ printfLine
                     : map (\line -> "  - \"echo '" ++ line ++ "' >> cabal.project\"")
                           (relevantCabalProjectLines ++ extraLines)
                    ++ go rest'

    relevantCabalProjectLines :: [String]
    relevantCabalProjectLines =
      let projContentLines = lines projContents
      in    collect "package"                   projContentLines
         ++ collect "source-repository-package" projContentLines

    collect :: String -> [String] -> [String]
    collect stanzaHead ls =
      let rest = dropWhile (\line -> not (stanzaHead `isPrefixOf` line) || (':' `elem` line))
                           ls
      in if null rest
         then []
         else let (stanzaHeaderLine:rest') = rest
                  (stanzaRest, rest'') =
                    span (\line -> case line of
                                     (x:_) -> isSpace x
                                     []    -> False)
                         rest'
              in    stanzaHeaderLine
                  : stanzaRest
                 ++ collect stanzaHead rest''

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

hacksMap :: Map Repo [Hack]
hacksMap = Map.fromList $ concat
  [ map (first (mkRepo "ekmett"))
    [ ("bits",          [hlint])
    , ("compensated",   [hlint])
    , ("contravariant", [hlint])
    , ("folds",         [hlint])
    , ("free",          [ CabalProjectMiscellanea
                          [ "package free-examples"
                          , "  flags: -mandelbrot-iter" -- Can't build HGL on Travis
                          ]
                        ])
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
    [ ("blank-canvas",      [CabalProjectMiscellanea [], DisableTestsGlobally])
    , ("data-reify",        [CabalProjectMiscellanea []])
    , ("dotgen",            [CabalProjectMiscellanea []])
    , ("javascript-bridge", [DisableTestsGlobally])
    , ("yampa-canvas",      [CabalProjectMiscellanea []])
    ]
  , [ (Repo "ku-fpg" "blank-canvas" (OtherBranch "0.6"), [CabalProjectMiscellanea [], DisableTestsGlobally]) ]

    -- Miscellaneous
  , [ (mkRepo "bos" "criterion",         [AlternateConfig ["-fembed-data-files"]])
    , (mkRepo "goldfirere" "singletons", [CabalProjectMiscellanea []])
    , (mkRepo "haskell" "primitive",     [CabalProjectMiscellanea []])
    , (mkRepo "RyanGlScott" "echo",      [CabalProjectMiscellanea []])
    ]
  ]
  where
    hlint :: Hack
    hlint = HLint ["HLINT"]
