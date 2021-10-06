{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Repos where

import           Cabal.Project
import qualified Data.Set.Ordered as OSet
import           Data.Set.Ordered (OSet)
import           Data.Void
import           Distribution.PackageDescription (GenericPackageDescription)
import           System.FilePath

data Repo = Repo
  { repoOwner  :: !String
  , repoName   :: !String
  , repoBranch :: !Branch
  } deriving stock (Eq, Ord, Read, Show)

data Branch
  = MainBranch
  | MasterBranch
  | OtherBranch !String
  deriving stock (Eq, Ord, Read, Show)

mkRepoMain :: String -> String -> Repo
mkRepoMain owner name =
  Repo{ repoOwner  = owner
      , repoName   = name
      , repoBranch = MainBranch
      }

mkRepoMaster :: String -> String -> Repo
mkRepoMaster owner name =
  Repo{ repoOwner  = owner
      , repoName   = name
      , repoBranch = MasterBranch
      }

mkRepoOther :: String -> (String, String) -> Repo
mkRepoOther owner (name, branch) =
  Repo{ repoOwner  = owner
      , repoName   = name
      , repoBranch = OtherBranch branch
      }

ppRepo :: Repo -> String
ppRepo r = repoURLSuffix r ++ " (" ++ branchName (repoBranch r) ++ " branch)"

repoURLSuffix :: Repo -> FilePath
repoURLSuffix Repo{repoOwner, repoName} = repoOwner </> repoName

repoFullSuffix :: Repo -> FilePath
repoFullSuffix r =
  let urlSuffix = repoURLSuffix r in
  case repoBranch r of
    MainBranch    -> urlSuffix
    MasterBranch  -> urlSuffix
    OtherBranch _ -> urlSuffix ++ "-" ++ branchName (repoBranch r)

branchName :: Branch -> String
branchName MainBranch         = "main"
branchName MasterBranch       = "master"
branchName (OtherBranch name) = name

data RepoMetadata = RM
  { rmRepo    :: !Repo
  , rmProject :: !(Project Void String String)
  }

data Component = Component
  { compName :: !String
  , compGpd  :: !GenericPackageDescription
  }

repos :: OSet Repo
repos = OSet.fromList $ concat
  [ map (mkRepoMaster "ekmett")
    [ "ad"
    , "adjunctions"
    , "approximate"
    , "bits"
    , "bound"
    , "bytes"
    , "charset"
    , "compensated"
    -- , "discrimination"
    , "either"
    , "eq"
    , "ersatz"
    , "exceptions"
    , "folds"
    , "free"
    , "gl"
    , "graphs"
    -- , "half"
    -- , "hash"
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
    -- , "promises"
    , "rcu"
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
    -- , "unique"
    , "vector-instances"
    , "void"
    , "zippers"
    ]
  , map (mkRepoMain "ekmett")
    [ "bifunctors"
    , "comonad"
    , "constraints"
    , "contravariant"
    , "distributive"
    , "profunctors"
    ]
  , map (mkRepoOther "ekmett")
    [ ("bifunctors",    "5")
    , ("comonad",       "5")
    , ("contravariant", "1.5")
    , ("distributive",  "0")
    , ("profunctors",   "5")
    ]

  , map (mkRepoMaster "goldfirere")
    [ "singletons"
    , "th-desugar"
    ]

  , map (mkRepoMaster "haskell-compat")
    [ "base-compat"
    , "base-orphans"
    , "deriving-compat"
    , "mtl-compat"
    , "th-compat"
    ]
  , map (mkRepoMain "haskell-compat")
    [ "ghc-bignum-orphans"
    ]

  {-
  , map (mkRepoMaster "haskellari")
    [ "cabal-doctest"
    ]
  -}

  , map (mkRepoMaster "ku-fpg")
    [ "blank-canvas"
    , "data-reify"
    , "dotgen"
    -- , "hood"
    , "javascript-bridge"
    , "kansas-comet"
    , "natural-transformation"
    , "yampa-canvas"
    ]
  , [ Repo "ku-fpg" "blank-canvas" (OtherBranch "0.7") ]

  , map (mkRepoMaster "mgsloan")
    [ "th-orphans"
    , "th-reify-many"
    ]

  , map (mkRepoMaster "rrnewton")
    [ -- "atomic-primops"
      "criterion-external"
    , "thread-local-storage"
    ]

  , map (mkRepoMaster "RyanGlScott")
    [ "code-page"
    , "constraint-tuples"
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
    , "th-lift"
    ]

  , map (mkRepoMaster "scotty-web")
    [ "wai-middleware-static"
    ]

  -- Miscellaneous
  , [ mkRepoMaster "AndrasKovacs" "singleton-nats"
    , mkRepoMaster "DanielSchuessler" "th-expand-syns"
    , mkRepoMaster "dreixel" "generic-deriving"
    , mkRepoMaster "foxik" "hashmap"
    , mkRepoMaster "glguy" "th-abstraction"
    , mkRepoMaster "haskell" "criterion"
    -- , mkRepoMaster "haskell-opengl" "StateVar"
    -- , mkRepoMaster "hesselink" "type-equality"
    , mkRepoMaster "lens" "lens-aeson"
    -- , mkRepoMaster "lpsmith" "bytestring-builder"
    , mkRepoMaster "nfrisby" "invariant-functors"
    -- , mkRepoMaster "recursion-schemes" "recursion-schemes"
    ]
  ]
