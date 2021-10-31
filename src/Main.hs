{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Repos

import           Cabal.Parse
import           Cabal.Project
import           Cabal.SourceRepo
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor (Bifunctor(..))
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Semigroup.Generic
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import           Data.Traversable
import           Distribution.Fields.Pretty (PrettyField, showFields)
import qualified Distribution.Package as Pkg
import           Distribution.PackageDescription (package, packageDescription)
import           Distribution.Pretty (prettyShow)
import           Distribution.Text (display, simpleParse)
import           Distribution.Types.PackageId (PackageIdentifier(..))
import           Distribution.Version
import           GHC.Generics (Generic)
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import           System.Process
import           Text.Regex.TDFA (Regex, makeRegex, matchM)

data Command
  = Pull !CommonOptions
  | Reset !CommonOptions
  | TestedWith !CommonOptions
  | Regenerate !CommonOptions
  | Diff !CommonOptions
  | Outdated !OutdatedOptions !CommonOptions
  | Commit !CommonOptions
  | Push !CommonOptions
  | Everything !OutdatedOptions !CommonOptions
  | Projectify !ProjectifyOptions !CommonOptions
  | Clean
  deriving stock (Eq, Show)

newtype OutdatedOptions = OutdatedOptions
  { excludeDeps :: Maybe [String]
  } deriving newtype Eq
    deriving stock   Show

newtype ProjectifyOptions = ProjectifyOptions
  { wError :: Bool
  } deriving newtype Eq
    deriving stock   Show

data CommonOptions = CommonOptions
  { includePackages :: Maybe [Pattern]
  , startAt         :: Maybe Pattern
  } deriving stock (Eq, Show)

cmdParser :: Parser Command
cmdParser = subparser
  ( command "pull"
      (info (pullCommand <**> helper)
            (progDesc "Pull everything"))
 <> command "reset"
      (info (resetCommand <**> helper)
            (progDesc "Reset working changes"))
 <> command "tested-with"
      (info (testedWithCommand <**> helper)
            (progDesc "Updated tested-with stanzas"))
 <> command "regenerate"
      (info (regenerateCommand <**> helper)
            (progDesc "Regenerate CI-related YAML files"))
 <> command "diff"
      (info (diffCommand <**> helper)
            (progDesc "Show the changes"))
 <> command "outdated"
      (info (outdatedCommand <**> helper)
            (progDesc "Check if any dependencies are outdated"))
 <> command "commit"
      (info (commitCommand <**> helper)
            (progDesc "Commit the changes"))
 <> command "push"
      (info (pushCommand <**> helper)
            (progDesc "Push the changes"))
 <> command "everything"
      (info (everythingCommand <**> helper)
            (progDesc "Fully update each library"))
 <> command "projectify"
      (info (projectifyCommand <**> helper)
            (progDesc "Create a cabal.project for the checkout directory"))
 <> command "clean"
      (info (pure Clean <**> helper)
            (progDesc "Clean working directory"))
  )

pullCommand :: Parser Command
pullCommand = Pull <$> commonOptions

resetCommand :: Parser Command
resetCommand = Reset <$> commonOptions

testedWithCommand :: Parser Command
testedWithCommand = TestedWith <$> commonOptions

regenerateCommand :: Parser Command
regenerateCommand = Regenerate <$> commonOptions

diffCommand :: Parser Command
diffCommand = Diff <$> commonOptions

outdatedCommand :: Parser Command
outdatedCommand = Outdated <$> outdatedOptions <*> commonOptions

outdatedOptions :: Parser OutdatedOptions
outdatedOptions = OutdatedOptions
  <$> (optional . csListOption)
      (  long "exclude-deps"
      <> metavar "DEP1,DEP2,..."
      <> help "Don't check for these packages when determining outdated dependencies" )

commitCommand :: Parser Command
commitCommand = Commit <$> commonOptions

pushCommand :: Parser Command
pushCommand = Push <$> commonOptions

everythingCommand :: Parser Command
everythingCommand = Everything <$> outdatedOptions <*> commonOptions

projectifyCommand :: Parser Command
projectifyCommand = Projectify <$> projectifyOptions <*> commonOptions

projectifyOptions :: Parser ProjectifyOptions
projectifyOptions = ProjectifyOptions
  <$> switch
      (  long "Werror"
      <> help "Enable -Werror for each local package"
      )

commonOptions :: Parser CommonOptions
commonOptions = CommonOptions
  <$> (optional . fmap (map compile) . csListOption)
      (  long "include-packages"
      <> short 'i'
      <> metavar "PATTERN1,PATTERN2,..."
      <> help "Only check these packages" )
  <*> (optional . fmap compile . strOption)
      (  long "start-at"
      <> short 's'
      <> metavar "PATTERN"
      <> help "Check packages, starting at the supplied one" )

-- | Comma-separated lists of arguments.
csListOption :: Mod OptionFields String -> Parser [String]
csListOption flags = splitAtElement ',' <$> strOption flags

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement x l =
  case l of
    []          -> []
    e:es | e==x -> split' es
    es          -> split' es
  where
    split' es = let (esx,esxs) = break (x==) es
                in esx : splitAtElement x esxs

main :: IO ()
main = execParser opts >>= ciMaintenance
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc spiel
     <> header spiel )

    spiel = "Make maintaining CI-related YAML files somewhat easier"

ciMaintenance :: Command -> IO ()
ciMaintenance cmd =
  case cmd of
    Pull cmmn               -> perPackageAction_ cmmn pull
    Reset cmmn              -> perPackageAction_ cmmn reset
    TestedWith cmmn         -> perPackageAction_ cmmn testedWith
    Regenerate cmmn         -> perPackageAction_ cmmn regenerate
    Diff cmmn               -> perPackageAction_ cmmn diff
    Outdated outOpts cmmn   -> perPackageAction_ cmmn (outdated outOpts)
    Commit cmmn             -> perPackageAction_ cmmn commit
    Push cmmn               -> perPackageAction_ cmmn push
    Everything outOpts cmmn -> perPackageAction_ cmmn (everything outOpts)

    Projectify projOpts cmmn -> projectify cmmn projOpts
    Clean -> removeDirectoryRecursive =<< getCheckoutDir

perPackageAction_ :: CommonOptions -> (RepoMetadata -> FilePath -> IO ()) -> IO ()
perPackageAction_ cmmn thing = void $ perPackageAction cmmn thing

perPackageAction :: CommonOptions -> (RepoMetadata -> FilePath -> IO a) -> IO [a]
perPackageAction CommonOptions{includePackages,startAt} thing =
  inCheckoutDir $ \dir -> do
    let repos' :: [Repo]
        repos' = dropWhile (\r -> maybe False (not . repoMatchesPattern r) startAt) $
                 filter shouldRunRepo $ toList repos
    when (null repos') $ do
      putStrLn "Could not find any repos matching the following patterns:"
      putStr "  "
      traverse_ (traverse_ $ \pat -> putStr $ ' ':decompile pat) includePackages
      putStrLn ""
      exitFailure
    printBanner '~'
    putStrLn $ "~~ ci-maintenace will look at the following repos:"
    putStrLn "~~"
    for_ repos' $ \r -> putStrLn $ "~~     " ++ ppRepo r
    printBanner '~'
    putStrLn ""
    for repos' $ \r -> do
      printBanner '='
      putStrLn $ "== " ++ ppRepo r
      printBanner '='
      let repoDir = dir </> repoFullSuffix r
      cloneRepo (repoURLSuffix r) repoDir (repoBranch r)
      res <- bracket_ (setCurrentDirectory repoDir)
                      (setCurrentDirectory dir *> putStrLn "")
                      (do let path = "cabal.project"
                          contents <- BS.readFile path
                          pf <- either (fail . renderParseError) pure $
                                parseProject path contents
                          thing (RM r pf) repoDir)
      pure res
  where
    repoMatchesPattern :: Repo -> Pattern -> Bool
    repoMatchesPattern r p = p `match` repoFullSuffix r

    shouldRunRepo :: Repo -> Bool
    shouldRunRepo r = maybe True (any (repoMatchesPattern r)) includePackages

cloneRepo :: String -> FilePath -> Branch -> IO ()
cloneRepo name repoDir branch = do
  exists <- doesDirectoryExist repoDir
  unless exists $ do
    createDirectoryIfMissing True repoDir
    callProcess "git" [ "clone"
                      , "git@github.com:" ++ name
                      , "--branch"
                      , branchName branch
                      , repoDir
                      ]

pull :: RepoMetadata -> FilePath -> IO ()
pull _ _ = do
  callProcess "git" [ "pull" ]
  callProcess "git" [ "submodule"
                    , "update"
                    , "--init"
                    , "--recursive"
                    ]

reset :: RepoMetadata -> FilePath -> IO ()
reset _ _  = do
  callProcess "git" [ "reset"
                    , "--hard"
                    , "HEAD"
                    ]
  callProcess "git" [ "clean"
                    , "-fdx"
                    ]

testedWith :: RepoMetadata -> FilePath -> IO ()
testedWith (RM _ pf) fp = do
  for_ (prjPackages pf ++ prjOptPackages pf) $ \package -> do
    let cabalFileDir = fp </> package
    exists <- doesDirectoryExist cabalFileDir
    when exists $ do
      cabalFiles <- filter (\f -> takeExtension f == ".cabal") <$>
                    listDirectory cabalFileDir
      appVeyorYmlFiles <- filter (== "appveyor.yml") <$>
                          listDirectory cabalFileDir
      case appVeyorYmlFiles of
        [appVeyorYmlFile] -> replaceTestedWith appVeyorMatrixHack
                                               (cabalFileDir </> appVeyorYmlFile)
        _                 -> pure ()
      case cabalFiles of
        [cabalFile] -> replaceTestedWith cabalTestedVersionsHack
                                         (cabalFileDir </> cabalFile)
        _           -> fail $ show cabalFiles
  where
    replaceTestedWith :: (String -> String) -> FilePath -> IO ()
    replaceTestedWith hack filePath = do
      fileContents <- TS.unpack <$> TS.readFile filePath
      let fileContents' = hack fileContents
      TS.writeFile filePath $ TS.pack fileContents'

    cabalTestedVersionsHack :: String -> String
    cabalTestedVersionsHack = go
      where
        go :: String -> String
        go s = case matchM re s of
                 Just (before, _matched :: String, after, groups)
                   |  [ghc, primary, secondary, tertiary] <- groups
                   ,  tertiary /= "*"
                   -> before ++ ghc ++ hackNum primary secondary ++ go after
                   |  otherwise
                   -> error $ "cabalTestedVersionsHack.go: " ++ show groups
                 Nothing -> s

        hackNum :: String -> String -> String
        hackNum prim sec = fst $ latestTestedWithFor prim sec

        re :: Regex
        re = makeRegex "(GHC == |GHC==|\\|\\| ==)([0-9]+)\\.([0-9]+)\\.([0-9]+)"

    appVeyorMatrixHack :: String -> String
    appVeyorMatrixHack = go
      where
        go :: String -> String
        go s = case matchM re s of
                 Just (before, _matched :: String, after, groups)
                   |  [primary, secondary, _tertiary, _quaternary] <- groups
                   -> before ++ "GHCVER: \"" ++ hackNum primary secondary
                                     ++ "\"" ++ go after
                   |  otherwise
                   -> error $ "appVeyorMatrixHack.go: " ++ show groups
                 Nothing -> s

        hackNum :: String -> String -> String
        hackNum prim sec =
          case latestTestedWithFor prim sec of
            (maj, mbQuat) -> maj ++ maybe "" (\quat -> '.':show quat) mbQuat

        re :: Regex
        re = makeRegex "GHCVER: \"([0-9]+)\\.([0-9]+)\\.([0-9]+)(\\.[0-9]+)?\""

    latestTestedWithFor :: String -> String -> (String, Maybe Int)
    latestTestedWithFor prim sec =
      case Map.lookup (prim, sec) supportedGhcVersions of
        Just (ver, appVeyorQuaternary) -> (prettyShow ver, appVeyorQuaternary)
        Nothing                        -> error $ show (prim, sec)

regenerate :: RepoMetadata -> FilePath -> IO ()
regenerate _ _ = do
  let haskellCIDir = "../../../haskell-ci"
  cloneRepo "haskell-CI/haskell-ci" haskellCIDir MasterBranch
  haskellCIExe <- withCurrentDirectory haskellCIDir $ do
    callProcess "cabal" [ "v2-build", "exe:haskell-ci" ]
    trim <$> readProcess "cabal" [ "list-bin", "haskell-ci" ] ""
  callProcess haskellCIExe [ "regenerate" ]

diff :: RepoMetadata -> FilePath -> IO ()
diff _ _ = gitDiff >>= putStrLn

outdated :: OutdatedOptions -> RepoMetadata -> FilePath -> IO ()
outdated OutdatedOptions{excludeDeps} RM{rmRepo = Repo{repoName}} _ =
  bracket_ (callProcess "cabal" [ "v2-freeze" ])
           (removeFile "cabal.project.freeze")
           go
  where
    go :: IO ()
    go = do
      globalPkgs <- readProcess "ghc-pkg" [ "list"
                                          , "--global"
                                          , "--simple-output"
                                          ] ""
      let globalPkgIds :: [PackageIdentifier]
          globalPkgIds = map (\x -> fromMaybe (error $ "Invaid package: " ++ x)
                                              (simpleParse x))
                             (words globalPkgs)
      (ec, stdout, _stderr)
        <- readProcessWithExitCode
           "cabal" [ "outdated"
                   , "--v2-freeze-file"
                   , "--exit-code"
                   , "--ignore=" ++ intercalate ","
                                    (  map (display . pkgName) globalPkgIds
                                    ++ fromMaybe [] excludeDeps )
                   ] ""
      case ec of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          putStrLn $ unwords
            [ repoName ++ " has outdated dependencies:"
            , stdout
            ]
          exitFailure

commit :: RepoMetadata -> FilePath -> IO ()
commit _ _ = do
  output <- gitDiff
  unless (null output) $ do
    printBanner '-'
    putStrLn "-- You have uncommitted changes."
    putStrLn "-- Commit and push? [y/n]"
    printBanner '-'
    response <- getLine
    if map toLower response == "y"
       then callProcess "git" [ "commit"
                              , "-a"
                              , "-m", commitTitle
                              , "-m", commitDescription
                              ]
       else do putStrLn "Come back when you're ready."
               exitFailure

push :: RepoMetadata -> FilePath -> IO ()
push (RM r _) _ =
  callProcess "git" [ "push"
                    , "origin"
                    , branchName $ repoBranch r
                    ]

everything :: OutdatedOptions -> RepoMetadata -> FilePath -> IO ()
everything outOpts r fp =
  traverse_ (\f -> f r fp)
    [ reset
    , pull
    , testedWith
    , regenerate
    , outdated outOpts
    , diff
    , commit
    , push
    ]

projectify :: CommonOptions -> ProjectifyOptions -> IO ()
projectify cmmn ProjectifyOptions{wError} = do
  cDir <- getCheckoutDir
  projInfos <- perPackageAction cmmn (gatherProjectInfo cDir)
  let project = displayProject $ mconcat projInfos
  putStrLn project -- Print it out as a quick sanity check
  TS.writeFile (cDir </> "cabal.project") $ TS.pack project
  where
    gatherProjectInfo :: FilePath -> RepoMetadata -> FilePath -> IO ProjectInfo
    gatherProjectInfo cDir
                      (RM _ prj@Project{ prjPackages, prjOptPackages
                                       , prjSourceRepos, prjOtherFields })
                      fp = do
      rslvPrj_either    <- resolveProject (fp </> "cabal.project") prj
      rslvPrj           <- either (fail . renderResolveError) pure rslvPrj_either
      cabalFiles_either <- readPackagesOfProject rslvPrj
      Project{prjPackages = cabalFiles}
                        <- either (fail . renderParseError) pure cabalFiles_either

      let pkgNameOf (_, gpd) = display $ Pkg.pkgName $ package $ packageDescription gpd

          prefix    = makeRelative cDir fp
          prepend p = normalise $ prefix </> p
      pure PI { piPackages     = map prepend prjPackages
              , piOptPackages  = map prepend prjOptPackages
              , piPackageNames = map pkgNameOf cabalFiles
              , piSourceRepos  = prjSourceRepos
              , piOtherFields  = prjOtherFields
              }

    displayProject :: ProjectInfo -> String
    displayProject (PI{ piPackages, piOptPackages, piPackageNames
                      , piSourceRepos, piOtherFields }) =
      unlines
        $ map ("packages: "          ++) piPackages
       ++ map ("optional-packages: " ++) piOptPackages
       ++ [ "package " ++ pkgName ++ "\n  ghc-options: -Werror"
          | wError, pkgName <- piPackageNames
          ]
       ++ map displaySourceRepo piSourceRepos
       ++ [showFields (const []) piOtherFields]

    displaySourceRepo :: SourceRepositoryPackage Maybe -> String
    displaySourceRepo (SourceRepositoryPackage{ srpType, srpLocation, srpTag
                                              , srpBranch, srpSubdir }) =
      unlines
        [ "source-repository-package"
        , "    type: "     ++ prettyShow srpType
        , "    location: " ++ srpLocation
        , displayMaybe srpTag    ("    tag: "    ++)
        , displayMaybe srpBranch ("    branch: " ++)
        , displayMaybe srpSubdir ("    subdir: " ++)
        ]

    displayMaybe :: Maybe a -> (a -> String) -> String
    displayMaybe = flip foldMap

-- The subset of cabal.project that we care about.
data ProjectInfo = PI
  { piPackages     :: [String]
  , piOptPackages  :: [String]
  , piPackageNames :: [String]
  , piSourceRepos  :: [SourceRepositoryPackage Maybe]
  , piOtherFields  :: [PrettyField ()]
  } deriving stock Generic
    deriving (Semigroup, Monoid)
             via GenericSemigroupMonoid ProjectInfo

gitDiff :: IO String
gitDiff = readProcess "git" [ "diff"
                            , "--color=always"
                            ] ""

commitTitle :: String
commitTitle = "Regenerate CI-related YAML files"

commitDescription :: String
commitDescription = unlines
  [ "This commit was performed automatically by a script."
  , "https://github.com/RyanGlScott/ci-maintenance"
  ]

inCheckoutDir :: (FilePath -> IO a) -> IO a
inCheckoutDir thing = do
  checkoutDir <- getCheckoutDir
  createDirectoryIfMissing True checkoutDir
  withCurrentDirectory checkoutDir (thing checkoutDir)

getCheckoutDir :: IO FilePath
getCheckoutDir = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "checkout"

supportedGhcVersions :: Map (String, String) (Version, Maybe Int)
-- The Version is the the latest point release, up to the tertiary version number.
-- The Maybe Int is the quaternary version number corresponding to the latest
-- AppVeyor version (if required).
supportedGhcVersions =
  Map.fromList $ map (fmap (first mkVersion))
  [ (("7","0"),  ([7,0,4],  Nothing))
  , (("7","2"),  ([7,2,2],  Nothing))
  , (("7","4"),  ([7,4,2],  Nothing))
  , (("7","6"),  ([7,6,3],  Just 1))
  , (("7","8"),  ([7,8,4],  Just 1))
  , (("7","10"), ([7,10,3], Just 2))
  , (("8","0"),  ([8,0,2],  Just 2))
  , (("8","2"),  ([8,2,2],  Nothing))
  , (("8","4"),  ([8,4,4],  Nothing))
  , (("8","6"),  ([8,6,5],  Nothing))
  , (("8","8"),  ([8,8,4],  Just 1))
  , (("8","10"), ([8,10,7], Nothing))
  , (("9","0"),  ([9,0,1],  Nothing))
  , (("9","2"),  ([9,2,1],  Nothing))
  ]

printBanner :: Char -> IO ()
printBanner = putStrLn . replicate 35
