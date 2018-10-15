{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Repos

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath
import System.Process

data Command
  = Refresh [FilePath]

cmdParser :: Parser Command
cmdParser = subparser
  ( command "refresh" (info (refreshOptions <**> helper) (progDesc "Pull everything"))
  )

refreshOptions :: Parser Command
refreshOptions = Refresh
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
    Refresh pkgs -> refresh pkgs

refresh :: [FilePath] -> IO ()
refresh pkgs =
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

inCheckoutDir :: (FilePath -> IO a) -> IO a
inCheckoutDir thing = do
  cwd <- getCurrentDirectory
  let checkoutDir = cwd </> "checkout"
  createDirectoryIfMissing True checkoutDir
  bracket_ (setCurrentDirectory checkoutDir)
           (setCurrentDirectory cwd)
           (thing checkoutDir)
