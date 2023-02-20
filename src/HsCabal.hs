module HsCabal
  ( listDirRec,
    listMod,
    getModules,
    (^>=),
    (@@),
    anyVersionDep,
    unConditional,
    warningOpt,
    simpleBuildInfo,
    addLibraryMod,
    LocalPackage (..),
    writeLocalPackage,
    simpleCabalProject,
  )
where

import Data.Foldable
import Data.Functor
import qualified Data.Set as S
import Distribution.Client.ProjectConfig
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.VersionRange
import Distribution.Utils.Path
import Distribution.Version
import Language.Haskell.Extension
import System.Directory
import System.FilePath

listDirRec :: FilePath -> IO (S.Set FilePath)
listDirRec root =
  listDirectory root >>= \ds ->
    withCurrentDirectory
      root
      ( foldrM
          (\p r -> fmap (\r1 -> r1 r) (go p))
          S.empty
          ds
      )
  where
    go :: FilePath -> IO (S.Set FilePath -> S.Set FilePath)
    go p = do
      isD <- doesDirectoryExist p
      if isD
        then
          listDirectory p
            >>= foldrM
              ( \f r -> do
                  s <- go (p </> f)
                  pure (s . r)
              )
              id
        else pure (S.insert p)

getModules :: String -> S.Set FilePath -> S.Set ModuleName
getModules ext =
  S.mapMonotonic
    (fromString . fmap (\c -> if c == '/' then '.' else c) . dropExtension)
    . S.filter (ext `isExtensionOf`)

listMod :: String -> FilePath -> IO (S.Set ModuleName)
listMod ext d = getModules ext <$> listDirRec d

(^>=) :: PackageName -> [Int] -> Dependency
d ^>= v =
  mkDependency
    d
    (majorBoundVersion (mkVersion v))
    mainLibSet

(@@) :: PackageName -> VersionRange -> Dependency
d @@ v = mkDependency d v mainLibSet

anyVersionDep :: PackageName -> Dependency
anyVersionDep d = mkDependency d anyVersion mainLibSet

warningOpt :: PerCompilerFlavor [String]
warningOpt =
  PerCompilerFlavor
    [ "-Wall",
      "-Wcompat",
      "-Widentities",
      "-Wincomplete-record-updates",
      "-Wincomplete-uni-patterns",
      "-Wmissing-export-lists",
      "-Wmissing-home-modules",
      "-Wpartial-fields",
      "-Wredundant-constraints"
    ]
    []

simpleBuildInfo :: FilePath -> PerCompilerFlavor [String] -> BuildInfo
simpleBuildInfo src opt =
  emptyBuildInfo
    { hsSourceDirs = [unsafeMakeSymbolicPath src],
      defaultLanguage = Just Haskell2010,
      options = warningOpt <> opt
    }

addLibraryMod :: Library -> IO Library
addLibraryMod lib = do
  (mods, sigs) <-
    traverse
      (listDirRec . getSymbolicPath)
      (hsSourceDirs (libBuildInfo lib))
      <&> ( \cont ->
              (getModules "hs" cont, getModules "hsig" cont)
          )
        . mconcat
  pure
    ( lib
        { exposedModules =
            S.toAscList
              ( mods
                  `S.difference` S.fromList (otherModules (libBuildInfo lib))
              ),
          signatures = S.toAscList sigs
        }
    )

unConditional :: Monoid c => a -> CondTree v c a
unConditional v =
  CondNode
    { condTreeData = v,
      condTreeConstraints = mempty,
      condTreeComponents = []
    }

data LocalPackage = LocalPackage
  { lpName :: PackageName,
    lpRoot :: FilePath,
    lpDesc :: GenericPackageDescription
  }
  deriving (Show, Eq)

writeLocalPackage :: FilePath -> LocalPackage -> IO ()
writeLocalPackage root lp =
  let dest = root </> lpRoot lp </> unPackageName (lpName lp) <.> "cabal"
   in putStrLn
        ( "Generate package description for "
            ++ show (lpName lp)
            ++ ": "
            ++ dest
        )
        >> writeGenericPackageDescription dest (lpDesc lp)

simpleCabalProject :: [LocalPackage] -> ProjectConfig
simpleCabalProject lp = mempty {projectPackages = fmap lpRoot lp}
