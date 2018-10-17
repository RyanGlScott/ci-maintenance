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
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import           Data.Traversable
import           Distribution.PackageDescription.Parsec
import           Distribution.Pretty (prettyShow)
import           Distribution.Verbosity (normal)
import           Distribution.Version
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

data Command
  = Pull [FilePath]
  | Reset [FilePath]
  | TestedWith [FilePath]
  | Regenerate [FilePath]
  | Diff [FilePath]
  | Commit [FilePath]
  | Push [FilePath]
  | Everything [FilePath]
  | Clean
  deriving (Eq, Ord, Read, Show)

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
pullOptions = Pull <$> manyPackages

resetOptions :: Parser Command
resetOptions = Reset <$> manyPackages

testedWithOptions :: Parser Command
testedWithOptions = TestedWith <$> manyPackages

regenerateOptions :: Parser Command
regenerateOptions = Regenerate <$> manyPackages

diffOptions :: Parser Command
diffOptions = Diff <$> manyPackages

commitOptions :: Parser Command
commitOptions = Commit <$> manyPackages

pushOptions :: Parser Command
pushOptions = Push <$> manyPackages

everythingOptions :: Parser Command
everythingOptions = Everything <$> manyPackages

manyPackages :: Parser [FilePath]
manyPackages = many $ argument str $ metavar "PACKAGE..."

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
    Pull pkgs       -> perPackageAction pkgs pull
    Reset pkgs      -> perPackageAction pkgs reset
    TestedWith pkgs -> perPackageAction pkgs testedWith
    Regenerate pkgs -> perPackageAction pkgs regenerate
    Diff pkgs       -> perPackageAction pkgs diff
    Commit pkgs     -> perPackageAction pkgs commit
    Push pkgs       -> perPackageAction pkgs push
    Everything pkgs -> perPackageAction pkgs everything
    Clean           -> removeDirectoryRecursive =<< getCheckoutDir

perPackageAction :: [FilePath] -> (RepoMetadata -> FilePath -> IO ()) -> IO ()
perPackageAction pkgs thing =
  inCheckoutDir $ \dir ->
  for_ repos $ \r@(Repo{repoOwner, repoName, repoBranch}) -> do
    let repoSuffix     = TS.unpack repoOwner </> TS.unpack repoName
        repoBranchName = TS.unpack (branchName repoBranch)
        repoDir = case repoBranch of
                   MasterBranch  -> dir </> repoSuffix
                   OtherBranch _ -> dir </> repoSuffix ++ "-" ++ repoBranchName
    when (null pkgs || any (`isInfixOf` repoDir) pkgs) $ do
      let banner = "==================================="
      putStrLn banner
      putStrLn $ "== " ++ repoSuffix ++ " (" ++ repoBranchName ++ " branch)"
      putStrLn banner
      cloneRepo repoSuffix repoDir repoBranch
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

cloneRepo :: String -> FilePath -> Branch -> IO ()
cloneRepo name repoDir branch = do
  exists <- doesDirectoryExist repoDir
  unless exists $ do
    createDirectoryIfMissing True repoDir
    callProcess "git" [ "clone"
                      , "git@github.com:" ++ name
                      , "--branch"
                      , TS.unpack (branchName branch)
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
    hack ('G':'H':'C':    '=':'=':    primaryNum:'.':secondaryNum:'.':_tertiaryNum:rest)
      = "GHC==" ++ hackNum [primaryNum] [secondaryNum] ++ hack rest
    hack ('G':'H':'C':    '=':'=':    primaryNum:'.':sn1:sn2:     '.':_tertiaryNum:rest)
      = "GHC==" ++ hackNum [primaryNum] [sn1, sn2]     ++ hack rest
    hack ('G':'H':'C':' ':'=':'=':' ':primaryNum:'.':secondaryNum:'.':_tertiaryNum:rest)
      = "GHC == " ++ hackNum [primaryNum] [secondaryNum] ++ hack rest
    hack ('G':'H':'C':' ':'=':'=':' ':primaryNum:'.':sn1:sn2:     '.':_tertiaryNum:rest)
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
                      -- TODO: Better path handling here
      oldMakeTravisYml = haskellCIDir </> "make_travis_yml_2.hs"
      newMakeTravisYml = haskellCIDir </> "MakeTravisYml.hs"
  cloneRepo "haskell-CI/haskell-ci" haskellCIDir MasterBranch
  exists <- doesFileExist oldMakeTravisYml
  when exists $ renameFile oldMakeTravisYml newMakeTravisYml
  callProcess "ghc" [ haskellCIDir </> "Main.hs"
                    , "-O2"
                    , "-i" ++ haskellCIDir
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

commit :: RepoMetadata -> FilePath -> IO ()
commit _ _ = do
  output <- gitDiff
  unless (null output) $ do
    let banner = "------------------------------------------"
    putStrLn banner
    putStrLn "-- You have uncommitted changes."
    putStrLn "-- Commit? [y/n]"
    putStrLn banner
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
                    , TS.unpack $ branchName $ repoBranch r
                    ]

everything :: RepoMetadata -> FilePath -> IO ()
everything r fp =
  traverse_ (\f -> f r fp)
    [ reset
    , pull
    , testedWith
    , regenerate
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
  , (("8","6"),  [8,6,1])
  ]
