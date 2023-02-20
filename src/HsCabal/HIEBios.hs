module HsCabal.HIEBios
  ( ProjectBuilder (..),
    projectCradle,
    simpleCabalCradle,
    simpleCradleConfig,
  )
where

import Data.Foldable
import Distribution.Client.Config
import Distribution.Client.DistDirLayout
import Distribution.Compiler
import Distribution.Version
import HIE.Bios.Config.YAML
import System.Info (fullCompilerVersion)

data ProjectBuilder = PbCabal
  deriving (Show, Eq, Ord)

projectCradle :: ProjectBuilder -> [String] -> [String] -> IO (CradleComponent a)
projectCradle PbCabal extraPkgs otherArgs = do
  cd <- getCabalDir
  pure
    ( Direct
        ( DirectConfig
            ( concat
                [ [ "-package-db",
                    (storePackageDBPath . cabalStoreDirLayout . defaultCabalDirLayout)
                      cd
                      (CompilerId GHC (mkVersion' fullCompilerVersion))
                  ],
                  foldMap'
                    (\p -> ["-package", p])
                    ("Cabal-hs" : extraPkgs),
                  otherArgs
                ]
            )
        )
    )

simpleCabalCradle :: CradleComponent a
simpleCabalCradle = Cabal (CabalConfig NoComponent)

simpleCradleConfig :: ProjectBuilder -> [String] -> [String] -> IO (CradleConfigYAML a)
simpleCradleConfig PbCabal dep oa = do
  pc <- projectCradle PbCabal dep oa
  pure
    ( CradleConfigYAML
        { cradle =
            Multi
              [ MultiSubComponent
                  { path = "Project.hs",
                    config =
                      CradleConfigYAML
                        { cradle = pc,
                          dependencies = Nothing
                        }
                  },
                MultiSubComponent
                  { path = ".",
                    config =
                      CradleConfigYAML
                        { cradle = simpleCabalCradle,
                          dependencies = Nothing
                        }
                  }
              ],
          dependencies = Nothing
        }
    )