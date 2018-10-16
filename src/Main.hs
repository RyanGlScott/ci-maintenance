{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import           CabalProjectParser
import           Repos

import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import           Distribution.Pretty (prettyShow)
import           Distribution.Version
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Process

data Command
  = Pull [FilePath]
  | TestedWith [FilePath]

cmdParser :: Parser Command
cmdParser = subparser
  ( command "pull"
      (info (pullOptions <**> helper)
            (progDesc "Pull everything"))
 <> command "tested-with"
      (info (testedWithOptions <**> helper)
            (progDesc "Updated tested-with stanzas"))
  )

pullOptions :: Parser Command
pullOptions = Pull
  <$> many (argument str (metavar "PACKAGE..."))

testedWithOptions :: Parser Command
testedWithOptions = TestedWith
  <$> many (argument str (metavar "PACKAGE..."))

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
    Pull pkgs -> pull pkgs
    TestedWith pkgs -> testedWith pkgs

pull :: [FilePath] -> IO ()
pull pkgs =
  inCheckoutDir $ \dir ->
  for_ repos $ \r -> do
    let repoSuffix = fullRepoName r
        repoDir    = dir </> repoSuffix
    when (null pkgs || any (`isInfixOf` repoDir) pkgs) $ do
      exists <- doesDirectoryExist repoDir
      unless exists $ do
        createDirectoryIfMissing True repoDir
        callProcess "git" [ "clone"
                          , "git@github.com:" ++ repoSuffix
                          , repoDir
                          ]
      bracket_ (setCurrentDirectory repoDir)
               (setCurrentDirectory dir)
               (callProcess "git" ["pull"])

testedWith :: [FilePath] -> IO ()
testedWith pkgs =
  inCheckoutDir $ \dir ->
  for_ repos $ \r -> do
    let repoSuffix = fullRepoName r
        repoDir    = dir </> repoSuffix
    when (null pkgs || any (`isInfixOf` repoDir) pkgs) $ do
      exists <- doesDirectoryExist repoDir
      when exists $ do
        bracket_ (setCurrentDirectory repoDir)
                 (setCurrentDirectory dir)
                 (go repoDir)
  where
    go :: FilePath -> IO ()
    go fp = do
      let path = "cabal.project"
      contents <- TS.unpack <$> TS.readFile path
      pf <- either fail pure $ parseProjectFile path contents
      for_ (prjPackages pf) $ \package -> do
        let cabalFileDir = fp </> package
        cabalFiles <- filter (\f -> takeExtension f == ".cabal") <$>
                      listDirectory cabalFileDir
        case cabalFiles of
          [cabalFile] -> replaceTestedWith (cabalFileDir </> cabalFile)
          _           -> fail $ show cabalFiles

    replaceTestedWith :: FilePath -> IO ()
    replaceTestedWith cabalFilePath = do
      cabalFileContents <- TS.unpack <$> TS.readFile cabalFilePath
      let cabalFileContents' = hack cabalFileContents
      TS.writeFile cabalFilePath (TS.pack cabalFileContents')

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

inCheckoutDir :: (FilePath -> IO a) -> IO a
inCheckoutDir thing = do
  cwd <- getCurrentDirectory
  let checkoutDir = cwd </> "checkout"
  createDirectoryIfMissing True checkoutDir
  bracket_ (setCurrentDirectory checkoutDir)
           (setCurrentDirectory cwd)
           (thing checkoutDir)

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
