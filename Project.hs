{- cabal:
    build-depends: base, Cabal-hs, filepath, containers, Cabal-syntax
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Set qualified as S
import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.License
import Distribution.PackageDescription
import Distribution.Utils.Path (unsafeMakeSymbolicPath)
import Distribution.Version
import HsCabal
import HsCabal.HIEBios
import HsCabal.HIEBios.Pretty
import Language.Haskell.Extension

main :: IO ()
main = do
  pkg <- do
    let name = "Cabal-hs"
    m <- listMod "hs" "src"
    pure
      ( LocalPackage
          { lpName = name,
            lpRoot = ".",
            lpDesc =
              emptyGenericPackageDescription
                { packageDescription =
                    emptyPackageDescription
                      { specVersion = CabalSpecV3_0,
                        package = PackageIdentifier name (mkVersion [0, 1, 0, 0]),
                        licenseRaw = Right MIT,
                        licenseFiles = [unsafeMakeSymbolicPath "LICENSE"],
                        maintainer = "dariankline@outlook.com",
                        category = "Development",
                        author = "Jose Jane",
                        buildTypeRaw = Just Simple
                      },
                  condLibrary =
                    Just
                      ( unConditional
                          ( emptyLibrary
                              { exposedModules = S.toAscList m,
                                libBuildInfo =
                                  emptyBuildInfo
                                    { hsSourceDirs = [unsafeMakeSymbolicPath "src"],
                                      defaultLanguage = Just Haskell2010,
                                      options = PerCompilerFlavor ["-Wall"] [],
                                      targetBuildDepends =
                                        [ "base" ^>= [4, 17],
                                          "Cabal" ^>= [3, 8],
                                          "Cabal-syntax" ^>= [3, 8],
                                          "cabal-install" ^>= [3, 8],
                                          "hie-bios" ^>= [0, 11],
                                          "filepath" ^>= [1, 4],
                                          "directory" ^>= [1, 3],
                                          "template-haskell" ^>= [2, 19],
                                          "containers" ^>= [0, 6],
                                          "yaml" ^>= [0, 11],
                                          "aeson" ^>= [2, 1],
                                          "bytestring" ^>= [0, 11]
                                        ]
                                    }
                              }
                          )
                      )
                }
          }
      )
  writeLocalPackage "." pkg
  simpleCradleConfig
    @()
    PbCabal
    [ "base",
      "filepath",
      "containers",
      "Cabal-syntax",
      "cabal-install"
    ]
    ["-Wall"]
    >>= writeCradleConfig "."