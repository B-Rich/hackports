module Main where

--import System.Environment (getArgs)

import Data.Monoid
         ( Monoid(mempty) )

import Distribution.Package
         ( PackageName(..), packageName, packageVersion
         , Dependency(..), thisPackageVersion )
import Distribution.ModuleName (ModuleName)
import Distribution.License (License)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.PackageDescription   as Available
--import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Simple.Configure (configCompilerAux)

import Distribution.Client.Config

import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler (Compiler,PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils (equating, comparing)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Version   (Version)
import Distribution.Verbosity (Verbosity, verbose)

import Distribution.Client.Types
         ( AvailablePackage(..), Repo, AvailablePackageDb(..) )
import Distribution.Client.Setup
         ( globalRepos, configPackageDB' )
import Distribution.Client.Utils
         ( mergeBy, MergeResult(..) )
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages )

import Data.List
         ( sortBy, groupBy, maximumBy )
import Data.Maybe
         ( listToMaybe, fromJust, fromMaybe, isJust )
import Control.Monad
         ( MonadPlus(mplus), join )
import Control.Exception
         ( assert )

-- | The info that we can display for each package. It is information per
-- package name and covers all installed and avilable versions.
--
data PackageDisplayInfo = PackageDisplayInfo {
    pkgname           :: PackageName,
    allInstalled      :: [InstalledPackageInfo],
    allAvailable      :: [AvailablePackage],
    latestInstalled   :: Maybe InstalledPackageInfo,
    latestAvailable   :: Maybe AvailablePackage,
    homepage          :: String,
    bugReports        :: String,
    sourceRepo        :: String,
    synopsis          :: String,
    description       :: String,
    category          :: String,
    license           :: License,
--    copyright         :: String, --TODO: is this useful?
    author            :: String,
    maintainer        :: String,
    dependencies      :: [Dependency],
    flags             :: [Available.Flag],
    hasLib            :: Bool,
    hasExe            :: Bool,
    executables       :: [String],
    modules           :: [ModuleName],
    haddockHtml       :: FilePath,
    haveTarball       :: Bool
  }

installedVersions :: PackageDisplayInfo -> [Version]
installedVersions = map packageVersion . allInstalled

availableVersions :: PackageDisplayInfo -> [Version]
availableVersions = map packageVersion . allAvailable

-- | We get the 'PackageDisplayInfo' by combining the info for the installed
-- and available versions of a package.
--
-- * We're building info about a various versions of a single named package so
-- the input package info records are all supposed to refer to the same
-- package name.
--
mergePackageInfo :: [InstalledPackageInfo]
                 -> [AvailablePackage]
                 -> PackageDisplayInfo
mergePackageInfo installedPkgs availablePkgs =
  assert (length installedPkgs + length availablePkgs > 0) $
  PackageDisplayInfo {
    pkgname      = combine packageName available
                           packageName installed,
    allInstalled = installedPkgs,
    allAvailable = availablePkgs,
    latestInstalled = latest installedPkgs,
    latestAvailable = latest availablePkgs,
    license      = combine Available.license    available
                           Installed.license    installed,
    maintainer   = combine Available.maintainer available
                           Installed.maintainer installed,
    author       = combine Available.author     available
                           Installed.author     installed,
    homepage     = combine Available.homepage   available
                           Installed.homepage   installed,
    bugReports   = maybe "" Available.bugReports available,
    sourceRepo   = fromMaybe "" . join
                 . fmap (uncons Nothing Available.repoLocation
                       . sortBy (comparing Available.repoKind)
                       . Available.sourceRepos)
                 $ available,
    synopsis     = combine Available.synopsis    available
                           Installed.description installed,
    description  = combine Available.description available
                           Installed.description installed,
    category     = combine Available.category    available
                           Installed.category    installed,
    flags        = maybe [] Available.genPackageFlags availableGeneric,
    hasLib       = isJust installed
                || fromMaybe False
                   (fmap (isJust . Available.condLibrary) availableGeneric),
    hasExe       = fromMaybe False
                   (fmap (not . null . Available.condExecutables) availableGeneric),
    executables  = map fst (maybe [] Available.condExecutables availableGeneric),
    modules      = combine Installed.exposedModules installed
                           (maybe [] Available.exposedModules
                                   . Available.library) available,
    dependencies = combine Available.buildDepends available
                           (map thisPackageVersion
                             . Installed.depends) installed,
    haddockHtml  = fromMaybe "" . join
                 . fmap (listToMaybe . Installed.haddockHTMLs)
                 $ installed,
    haveTarball  = False
  }
  where
    combine f x g y  = fromJust (fmap f x `mplus` fmap g y)
    installed        = latest installedPkgs
    availableGeneric = fmap packageDescription (latest availablePkgs)
    available        = fmap flattenPackageDescription availableGeneric
    latest []        = Nothing
    latest pkgs      = Just (maximumBy (comparing packageVersion) pkgs)

    uncons :: b -> (a -> b) -> [a] -> b
    uncons z _ []    = z
    uncons _ f (x:_) = f x

-- | Rearrange installed and available packages into groups referring to the
-- same package by name. In the result pairs, the lists are guaranteed to not
-- both be empty.
--
mergePackages ::   [InstalledPackageInfo] -> [AvailablePackage]
              -> [([InstalledPackageInfo],   [AvailablePackage])]
mergePackages installed available =
    map collect
  $ mergeBy (\i a -> fst i `compare` fst a)
            (groupOn packageName installed)
            (groupOn packageName available)
  where
    collect (OnlyInLeft  (_,is)       ) = (is, [])
    collect (    InBoth  (_,is) (_,as)) = (is, as)
    collect (OnlyInRight        (_,as)) = ([], as)

groupOn :: Ord key => (a -> key) -> [a] -> [(key,[a])]
groupOn key = map (\xs -> (key (head xs), xs))
            . groupBy (equating key)
            . sortBy (comparing key)

getPackages :: Verbosity
     -> PackageDB
     -> [Repo]
     -> Compiler
     -> ProgramConfiguration
     -> IO [PackageDisplayInfo]
getPackages verbosity packageDB repos comp conf = do
    Just installed <- getInstalledPackages verbosity comp packageDB conf
    AvailablePackageDb available _ <- getAvailablePackages verbosity repos
    let pkgs = (PackageIndex.allPackages installed
               ,PackageIndex.allPackages available)
    return $ map (uncurry mergePackageInfo) $ uncurry mergePackages pkgs

allPackages :: Verbosity -> IO [PackageDisplayInfo]
allPackages verbosity = do
  config <- loadConfig verbosity mempty mempty
  let flgs = savedConfigureFlags config
  (comp, conf) <- configCompilerAux flgs
  getPackages verbosity (configPackageDB' flgs)
       (globalRepos (savedGlobalFlags config)) comp conf

main :: IO ()
main = do
  -- args <- getArgs
  pkgs <- allPackages verbose
  print $ map maintainer pkgs
