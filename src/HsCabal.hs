module HsCabal
  ( listDirRec,
    listMod,
    (^>=),
    (@@),
    anyVersionDep,
    unConditional,
    LocalPackage (..),
    writeLocalPackage,
    simpleCabalProject,
  )
where

import Data.Foldable
import qualified Data.Set as S
import Distribution.Client.ProjectConfig
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint
import Distribution.Types.VersionRange
import Distribution.Version
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

listMod :: String -> FilePath -> IO (S.Set ModuleName)
listMod ext d =
  S.mapMonotonic
    (fromString . fmap (\c -> if c == '/' then '.' else c) . dropExtension)
    . S.filter (ext `isExtensionOf`)
    <$> listDirRec d

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
