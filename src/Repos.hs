{-# LANGUAGE NamedFieldPuns #-}
module Repos where

import qualified Data.Set.Ordered as OSet
import           Data.Set.Ordered (OSet)
import           Distribution.PackageDescription (GenericPackageDescription)
import           HaskellCI.Project
import           System.FilePath

data Repo = Repo
  { repoOwner  :: !String
  , repoName   :: !String
  , repoBranch :: !Branch
  } deriving (Eq, Ord, Read, Show)

data Branch
  = MasterBranch
  | OtherBranch !String
  deriving (Eq, Ord, Read, Show)

mkRepo :: String -> String -> Repo
mkRepo owner name =
  Repo{ repoOwner  = owner
      , repoName   = name
      , repoBranch = MasterBranch
      }

ppRepo :: Repo -> String
ppRepo r = repoURLSuffix r ++ " (" ++ branchName (repoBranch r) ++ " branch)"

repoURLSuffix :: Repo -> FilePath
repoURLSuffix Repo{repoOwner, repoName} = repoOwner </> repoName

repoFullSuffix :: Repo -> FilePath
repoFullSuffix r =
  let urlSuffix = repoURLSuffix r in
  case repoBranch r of
    MasterBranch  -> urlSuffix
    OtherBranch _ -> urlSuffix ++ "-" ++ branchName (repoBranch r)

branchName :: Branch -> String
branchName MasterBranch       = "master"
branchName (OtherBranch name) = name

data RepoMetadata = RM
  { rmRepo    :: !Repo
  , rmProject :: !(Project String String)
  }

data Component = Component
  { compName :: !String
  , compGpd  :: !GenericPackageDescription
  }

repos :: OSet Repo
repos = OSet.fromList $ concat
  [ map (mkRepo "ekmett")
    [ "ad"
    , "adjunctions"
    , "approximate"
    , "bifunctors"
    , "bits"
    , "bound"
    , "bytes"
    , "charset"
    , "comonad"
    , "compensated"
    , "constraints"
    , "contravariant"
    , "distributive"
    , "either"
    , "eq"
    , "ersatz"
    , "exceptions"
    , "folds"
    , "free"
    -- , "gl"
    , "graphs"
    , "heaps"
    , "hybrid-vectors"
    , "hyperloglog"
    , "hyphenation"
    , "intern"
    , "intervals"
    , "kan-extensions"
    , "keys"
    , "lca"
    , "lens-action"
    , "lens"
    , "linear"
    , "log-domain"
    , "machines"
    , "nats"
    , "parsers"
    , "pointed"
    , "profunctors"
    , "rcu"
    , "recursion-schemes"
    , "reducers"
    , "reflection"
    , "semigroupoids"
    , "semigroups"
    , "streams"
    , "structs"
    , "tagged-transformer"
    , "tagged"
    , "transformers-compat"
    , "trifecta"
    , "vector-instances"
    , "void"
    , "zippers"
    ]

  , map (mkRepo "goldfirere")
    [ "singletons"
    , "th-desugar"
    ]

  , map (mkRepo "haskell")
    [ "hsc2hs"
    , "mtl"
    , "parallel"
    , "primitive"
    , "stm"
    , "unix"
    -- , "vector"
    ]

  , map (mkRepo "haskell-compat")
    [ "base-compat"
    , "base-orphans"
    , "deriving-compat"
    , "mtl-compat"
    ]

  , map (mkRepo "ku-fpg")
    [ "blank-canvas"
    , "data-reify"
    , "dotgen"
    -- , "hood"
    , "javascript-bridge"
    , "kansas-comet"
    , "natural-transformation"
    , "remote-monad"
    , "yampa-canvas"
    ]
  , [ Repo "ku-fpg" "blank-canvas" (OtherBranch "0.6") ]

  , map (mkRepo "rrnewton")
    [ -- "atomic-primops"
      "criterion-external"
    , "thread-local-storage"
    ]

  , map (mkRepo "RyanGlScott")
    [ "code-page"
    , "echo"
    , "eliminators"
    , "ghc-software-foundations"
    , "gists"
    , "keycode"
    , "lift-generics"
    , "mintty"
    , "proxied"
    , "singleton-gadts"
    , "text-show"
    , "text-show-instances"
    ]

  , map (mkRepo "scotty-web")
    [ "scotty"
    , "wai-middleware-static"
    ]

  -- Miscellaneous
  , [ mkRepo "AndrasKovacs" "singleton-nats"
    , mkRepo "bos" "criterion"
    , mkRepo "dreixel" "generic-deriving"
    , mkRepo "foxik" "hashmap"
    , mkRepo "glguy" "th-abstraction"
    , mkRepo "lens" "lens-aeson"
    -- , mkRepo "lpsmith" "bytestring-builder"
    , mkRepo "mgsloan" "th-orphans"
    , mkRepo "nfrisby" "invariant-functors"
    -- , mkRepo "phadej" "cabal-doctest"
    ]
  ]
