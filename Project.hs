{- cabal:
    build-depends: base, Cabal-hs, Cabal-syntax
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Distribution.CabalSpecVersion
import Distribution.License
import Distribution.PackageDescription
import Distribution.Utils.Path (unsafeMakeSymbolicPath)
import Distribution.Version
import HsCabal
import HsCabal.HIEBios
import HsCabal.HIEBios.Pretty

main :: IO ()
main = do
  lib <-
    addLibraryMod
      ( emptyLibrary
          { libBuildInfo =
              (simpleBuildInfo "src" mempty)
                { targetBuildDepends =
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
  writeLocalPackage
    "."
    ( let name = "Cabal-hs"
       in LocalPackage
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
                    condLibrary = Just (unConditional lib)
                  }
            }
    )
  simpleCradleConfig
    @()
    PbCabal
    ["base", "Cabal-syntax"]
    ["-Wall"]
    >>= writeCradleConfig "."