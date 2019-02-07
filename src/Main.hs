{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           CabalProjectParser
import           Hacks
import           Repos

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import qualified Data.Set.Ordered as OSet
import           Data.Set.Ordered (OSet)
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import           Data.Traversable
import           Distribution.PackageDescription.Parsec
import           Distribution.Pretty (prettyShow)
import           Distribution.Text (display, simpleParse)
import           Distribution.Types.PackageId (PackageIdentifier(..))
import           Distribution.Verbosity (normal)
import           Distribution.Version
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

data Command
  = Pull Common
  | Reset Common
  | TestedWith Common
  | Regenerate Common
  | Diff Common
  | Outdated Common
  | Commit Common
  | Push Common
  | Everything Common
  | Clean
  deriving (Eq, Ord, Read, Show)

newtype Common = Common
  { packages :: [String]
  } deriving (Eq, Ord, Read, Show)

cmdParser :: Parser Command
cmdParser = subparser
  ( command "pull"
      (info (pullOptions <**> helper)
            (progDesc "Pull everything"))
 <> command "reset"
      (info (resetOptions <**> helper)
            (progDesc "Reset working changes"))
 <> command "tested-with"
      (info (testedWithOptions <**> helper)
            (progDesc "Updated tested-with stanzas"))
 <> command "regenerate"
      (info (regenerateOptions <**> helper)
            (progDesc "Regenerate .travis.yml"))
 <> command "diff"
      (info (diffOptions <**> helper)
            (progDesc "Show the changes"))
 <> command "outdated"
      (info (outdatedOptions <**> helper)
            (progDesc "Check if any dependencies are outdated"))
 <> command "commit"
      (info (commitOptions <**> helper)
            (progDesc "Commit the changes"))
 <> command "push"
      (info (pushOptions <**> helper)
            (progDesc "Push the changes"))
 <> command "everything"
      (info (everythingOptions <**> helper)
            (progDesc "Fully update each library"))
 <> command "clean"
      (info (pure Clean <**> helper)
            (progDesc "Clean working directory"))
  )

pullOptions :: Parser Command
pullOptions = Pull <$> commonOptions

resetOptions :: Parser Command
resetOptions = Reset <$> commonOptions

testedWithOptions :: Parser Command
testedWithOptions = TestedWith <$> commonOptions

regenerateOptions :: Parser Command
regenerateOptions = Regenerate <$> commonOptions

diffOptions :: Parser Command
diffOptions = Diff <$> commonOptions

outdatedOptions :: Parser Command
outdatedOptions = Outdated <$> commonOptions

commitOptions :: Parser Command
commitOptions = Commit <$> commonOptions

pushOptions :: Parser Command
pushOptions = Push <$> commonOptions

everythingOptions :: Parser Command
everythingOptions = Everything <$> commonOptions

commonOptions :: Parser Common
commonOptions = Common <$> (many $ argument str $ metavar "PACKAGE...")

main :: IO ()
main = execParser opts >>= travisMaintenance
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> progDesc spiel
     <> header spiel )

    spiel = "Make maintaining .travis.yml files somewhat easier"

travisMaintenance :: Command -> IO ()
travisMaintenance cmd =
  case cmd of
    Pull cmmn       -> perPackageAction cmmn pull
    Reset cmmn      -> perPackageAction cmmn reset
    TestedWith cmmn -> perPackageAction cmmn testedWith
    Regenerate cmmn -> perPackageAction cmmn regenerate
    Diff cmmn       -> perPackageAction cmmn diff
    Outdated cmmn   -> perPackageAction cmmn outdated
    Commit cmmn     -> perPackageAction cmmn commit
    Push cmmn       -> perPackageAction cmmn push
    Everything cmmn -> perPackageAction cmmn everything
    Clean           -> removeDirectoryRecursive =<< getCheckoutDir

perPackageAction :: Common -> (RepoMetadata -> FilePath -> IO ()) -> IO ()
perPackageAction Common{packages} thing =
  inCheckoutDir $ \dir -> do
    let repos' :: OSet Repo
        repos' = OSet.filter shouldRunRepo repos
    printBanner '~'
    putStrLn $ "~~ travis-maintenace will look at the following repos:"
    putStrLn "~~"
    for_ repos' $ \r -> putStrLn $ "~~     " ++ ppRepo r
    printBanner '~'
    putStrLn ""
    for_ repos' $ \r -> do
      printBanner '='
      putStrLn $ "== " ++ ppRepo r
      printBanner '='
      let repoDir = dir </> repoFullSuffix r
      cloneRepo (repoURLSuffix r) repoDir (repoBranch r)
      bracket_ (setCurrentDirectory repoDir)
               (setCurrentDirectory dir *> putStrLn "")
               (do let path = "cabal.project"
                   contents <- TS.unpack <$> TS.readFile path
                   pf <- either fail pure $ parseProjectFile path contents
                   components <-
                     for (prjPackages pf) $ \package -> do
                       let cabalFileDir = repoDir </> package
                       cabalFiles <- filter (\f -> takeExtension f == ".cabal") <$>
                                     listDirectory cabalFileDir
                       case cabalFiles of
                         [cabalFile] -> do
                           gpd <- readGenericPackageDescription
                                    normal (cabalFileDir </> cabalFile)
                           pure Component{ compName = takeBaseName cabalFile
                                         , compGpd  = gpd
                                         }
                         _ -> fail $ show cabalFiles
                   thing (RM r pf contents components) repoDir)
  where
    shouldRunRepo :: Repo -> Bool
    shouldRunRepo r =
      null packages || any (`isInfixOf` repoFullSuffix r) packages


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
testedWith (RM _ pf _ _) fp = do
  for_ (prjPackages pf) $ \package -> do
    let cabalFileDir = fp </> package
    cabalFiles <- filter (\f -> takeExtension f == ".cabal") <$>
                  listDirectory cabalFileDir
    case cabalFiles of
      [cabalFile] -> replaceTestedWith (cabalFileDir </> cabalFile)
      _           -> fail $ show cabalFiles
  where
    replaceTestedWith :: FilePath -> IO ()
    replaceTestedWith cabalFilePath = do
      cabalFileContents <- TS.unpack <$> TS.readFile cabalFilePath
      let cabalFileContents' = hack cabalFileContents
      TS.writeFile cabalFilePath $ TS.pack cabalFileContents'

    hack :: String -> String
    hack [] = []
    hack ('G':'H':'C':    '=':'=':    primaryNum:'.':secondaryNum:'.':tertiaryNum:rest)
      | tertiaryNum /= '*'
      = "GHC==" ++ hackNum [primaryNum] [secondaryNum] ++ hack rest
    hack ('G':'H':'C':    '=':'=':    primaryNum:'.':sn1:sn2:     '.':tertiaryNum:rest)
      | tertiaryNum /= '*'
      = "GHC==" ++ hackNum [primaryNum] [sn1, sn2]     ++ hack rest
    hack ('G':'H':'C':' ':'=':'=':' ':primaryNum:'.':secondaryNum:'.':tertiaryNum:rest)
      | tertiaryNum /= '*'
      = "GHC == " ++ hackNum [primaryNum] [secondaryNum] ++ hack rest
    hack ('G':'H':'C':' ':'=':'=':' ':primaryNum:'.':sn1:sn2:     '.':tertiaryNum:rest)
      | tertiaryNum /= '*'
      = "GHC == " ++ hackNum [primaryNum] [sn1, sn2]     ++ hack rest
    hack (x:xs) = x:hack xs

    hackNum :: String -> String -> String
    hackNum prim sec =
      case Map.lookup (prim, sec) supportedGhcVersions of
        Just ver -> prettyShow ver
        Nothing -> error $ show (prim, sec)

regenerate :: RepoMetadata -> FilePath -> IO ()
regenerate rm fp = do
  let travisYml     = fp </> ".travis.yml"
      haskellCIDir  = "../../../haskell-ci"
  cloneRepo "haskell-CI/haskell-ci" haskellCIDir MasterBranch
  callProcess "ghc" [ haskellCIDir </> "Main.hs"
                    , "-O2"
                    , "-i" ++ haskellCIDir </> "src"
                    ]
  callProcess (haskellCIDir </> "Main")
                    [ "regenerate"
                    , travisYml
                    ]
  travisYmlContents <- TS.unpack <$> TS.readFile travisYml
  let mbTravisYmlContents' = applyHacks rm travisYmlContents
  case mbTravisYmlContents' of
    Nothing                 -> pure ()
    Just travisYmlContents' -> TS.writeFile travisYml $ TS.pack travisYmlContents'

diff :: RepoMetadata -> FilePath -> IO ()
diff _ _ = gitDiff >>= putStrLn

outdated :: RepoMetadata -> FilePath -> IO ()
outdated (RM{rmRepo = Repo{repoName}}) _ =
  bracket_ (callProcess "cabal" [ "new-freeze" ])
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
                   , "--new-freeze-file"
                   , "--exit-code"
                   , "--ignore=" ++ intercalate "," (map (display . pkgName) globalPkgIds)
                   ] ""
      case ec of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          putStrLn $ unwords
            [ repoName ++ "has outdated dependencies:"
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
push (RM r _ _ _) _ =
  callProcess "git" [ "push"
                    , "origin"
                    , branchName $ repoBranch r
                    ]

everything :: RepoMetadata -> FilePath -> IO ()
everything r fp =
  traverse_ (\f -> f r fp)
    [ reset
    , pull
    , testedWith
    , regenerate
    , outdated
    , diff
    , commit
    , push
    ]

gitDiff :: IO String
gitDiff = readProcess "git" [ "diff"
                            , "--color=always"
                            ] ""

commitTitle :: String
commitTitle = "Regenerate .travis.yml"

commitDescription :: String
commitDescription = unlines
  [ "This commit was performed automatically by a script."
  , "https://github.com/RyanGlScott/travis-maintenance"
  ]

inCheckoutDir :: (FilePath -> IO a) -> IO a
inCheckoutDir thing = do
  cwd <- getCurrentDirectory
  checkoutDir <- getCheckoutDir
  createDirectoryIfMissing True checkoutDir
  bracket_ (setCurrentDirectory checkoutDir)
           (setCurrentDirectory cwd)
           (thing checkoutDir)

getCheckoutDir :: IO FilePath
getCheckoutDir = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "checkout"

supportedGhcVersions :: Map (String, String) Version
supportedGhcVersions =
  Map.fromList $ map (fmap mkVersion)
  [ (("7","0"),  [7,0,4])
  , (("7","2"),  [7,2,2])
  , (("7","4"),  [7,4,2])
  , (("7","6"),  [7,6,3])
  , (("7","8"),  [7,8,4])
  , (("7","10"), [7,10,3])
  , (("8","0"),  [8,0,2])
  , (("8","2"),  [8,2,2])
  , (("8","4"),  [8,4,4])
  , (("8","6"),  [8,6,3])
  ]

printBanner :: Char -> IO ()
printBanner = putStrLn . replicate 35
