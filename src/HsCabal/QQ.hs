module HsCabal.QQ (versionRangeQ) where

import Distribution.Parsec (eitherParsec)
import Distribution.Types.VersionRange
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

versionRangeQ :: QuasiQuoter
versionRangeQ =
  QuasiQuoter
    { quoteExp = \s -> case eitherParsec s of
        Right v -> liftData (v :: VersionRange)
        Left e -> fail ("parse failed:" ++ e),
      quotePat = error "quote pat is not allowed",
      quoteType = error "quote type is not allowed",
      quoteDec = error "quote decl is not allowed"
    }