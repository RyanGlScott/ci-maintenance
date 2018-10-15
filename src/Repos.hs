{-# LANGUAGE OverloadedStrings #-}
module Repos where

import Data.Text (Text)

data Repo = Repo
  { repoOwner :: !Text
  , repoName  :: !Text
  } deriving (Eq, Ord, Read, Show)

repos :: [Repo]
repos = concat
  [ map (Repo "ekmett")
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

  , map (Repo "goldfirere")
    [ "singletons"
    , "th-desugar"
    ]

  , map (Repo "haskell")
    [ "hsc2hs"
    , "mtl"
    , "parallel"
    , "primitive"
    , "stm"
    , "unix"
    ]

  , map (Repo "haskell-compat")
    [ "base-compat"
    , "base-orphans"
    , "deriving-compat"
    , "mtl-compat"
    ]

  , map (Repo "ku-fpg")
    [ "blank-canvas"
    , "data-reify"
    , "dotgen"
    -- , "hood"
    , "javascript-bridge"
    , "kansas-comet"
    , "natural-transformation"
    , "yampa-canvas"
    ]

  , map (Repo "rrnewton")
    [ -- "atomic-primops"
      "criterion-external"
    , "thread-local-storage"
    ]

  , map (Repo "RyanGlScott")
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

  , map (Repo "scotty-web")
    [ "scotty"
    , "wai-middleware-static"
    ]

  -- Miscellaneous
  , [ Repo "AndrasKovacs" "singleton-nats"
    , Repo "bos" "criterion"
    , Repo "dreixel" "generic-deriving"
    , Repo "foxik" "hashmap"
    , Repo "glguy" "th-abstraction"
    , Repo "lens" "lens-aeson"
    -- , Repo "lpsmith" "bytestring-builder"
    , Repo "mgsloan" "th-orphans"
    , Repo "nfrisby" "invariant-functors"
    -- , Repo "phadej" "cabal-doctest"
    ]
  ]
