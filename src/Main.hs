{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Repos

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Text (unpack)
import Options.Applicative
import System.Directory
import System.FilePath
import System.Process

data Command
  = Refresh

cmdParser :: Parser Command
cmdParser = subparser
  ( command "refresh" (info refreshOptions (progDesc "Pull everything"))
  )

refreshOptions :: Parser Command
refreshOptions = pure Refresh

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
    Refresh -> refresh

refresh :: IO ()
refresh = inCheckoutDir $ \dir ->
          for_ repos $ \Repo{repoOwner, repoName} -> do
  let repoSuffix = unpack repoOwner </> unpack repoName
      repoDir    = dir </> repoSuffix
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
