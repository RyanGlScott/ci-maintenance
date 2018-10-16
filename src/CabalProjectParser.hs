-- Much of this is cargo-culted from haskell-ci.
module CabalProjectParser where

import Data.Char (isSpace)
import Data.Functor (void)
import Distribution.Compat.ReadP
    ( ReadP, (<++), (+++), between, char
    , many1, munch1, readS_to_P , gather )
import qualified Distribution.ParseUtils as PU

newtype Project = Project
  { prjPackages :: [FilePath]
  } deriving (Eq, Ord, Read, Show)

emptyProject :: Project
emptyProject = Project []

-- | Parse project file. Extracts only @packages@ field.
--
-- >>> fmap prjPackages $ parseProjectFile "cabal.project" "packages: foo bar/*.cabal"
-- Right ["foo","bar/*.cabal"]
--
parseProjectFile :: FilePath -> String -> Either String Project
parseProjectFile path contents =
    case PU.parseFields projectConfigFieldDescrs emptyProject contents of
        PU.ParseOk _ x -> Right x
        PU.ParseFailed err -> Left $ case PU.locatedErrorMsg err of
            (l, msg) -> "ERROR " ++ path ++ ":" ++ show l ++ ": " ++ msg

projectConfigFieldDescrs :: [PU.FieldDescr Project]
projectConfigFieldDescrs =
  [ PU.listField "packages"
      (error "no pretty-printing")
      parsePackageLocationTokenQ
      prjPackages
      (\x prj -> prj { prjPackages = x })
  ]

-- | This is a bit tricky since it has to cover globs which have embedded @,@
-- chars. But we don't just want to parse strictly as a glob since we want to
-- allow http urls which don't parse as globs, and possibly some
-- system-dependent file paths. So we parse fairly liberally as a token, but
-- we allow @,@ inside matched @{}@ braces.
--
parsePackageLocationTokenQ :: ReadP r String
parsePackageLocationTokenQ = parseHaskellString <++ parsePackageLocationToken
  where
    parsePackageLocationToken :: ReadP r String
    parsePackageLocationToken = fmap fst (gather outerTerm)
      where
        outerTerm   = alternateEither1 outerToken (braces innerTerm)
        innerTerm   = alternateEither  innerToken (braces innerTerm)
        outerToken  = void $ munch1 outerChar
        innerToken  = void $ munch1 innerChar
        outerChar c = not (isSpace c || c == '{' || c == '}' || c == ',')
        innerChar c = not (isSpace c || c == '{' || c == '}')
        braces      = between (char '{') (char '}')

    alternateEither, alternateEither1,
      alternatePQs, alternate1PQs, alternateQsP, alternate1QsP
      :: ReadP r () -> ReadP r () -> ReadP r ()

    alternateEither1 p q = alternate1PQs p q +++ alternate1QsP q p
    alternateEither  p q = alternateEither1 p q +++ return ()
    alternate1PQs    p q = p >> alternateQsP q p
    alternatePQs     p q = alternate1PQs p q +++ return ()
    alternate1QsP    q p = many1 q >> alternatePQs p q
    alternateQsP     q p = alternate1QsP q p +++ return ()

parseHaskellString :: ReadP r String
parseHaskellString = readS_to_P reads
