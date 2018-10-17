{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Repos where

import           CabalProjectParser

import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Text (Text)
import           Distribution.PackageDescription (GenericPackageDescription)

data Repo = Repo
  { repoOwner  :: !Text
  , repoName   :: !Text
  , repoBranch :: !Branch
  } deriving (Eq, Ord, Read, Show)

data Branch
  = MasterBranch
  | OtherBranch !Text
  deriving (Eq, Ord, Read, Show)

mkRepo :: Text -> Text -> Repo
mkRepo owner name =
  Repo{ repoOwner  = owner
      , repoName   = name
      , repoBranch = MasterBranch
      }

branchName :: Branch -> Text
branchName MasterBranch       = "master"
branchName (OtherBranch name) = name

data RepoMetadata = RM
  { rmRepo           :: !Repo
  , rmProject        :: !Project
  , rmComponentNames :: ![Component]
  }

data Component = Component
  { compName :: !String
  , compGpd  :: !GenericPackageDescription
  }

repos :: Set Repo
repos = Set.fromList $ concat
  [ map (mkRepo "ekmett")
    [ "ad"
    , "adjunctions"
    , "approximate"
    , "bifunctors"
    , "bits"
    , "bound"
    , "bytes"
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
    , "gc"
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
    ]

  , map (mkRepo "haskell-compat")
    [ -- "base-compat"
      "base-orphans"
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
    , "keycode"
    , "lift-generics"
    , "mintty"
    , "proxied"
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
