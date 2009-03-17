import Data.List
import System.Environment (getArgs)

import Distribution.Verbosity (verbose)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)

readDotCabal :: FilePath -> IO GenericPackageDescription
readDotCabal = readPackageDescription verbose

main = do
  args <- getArgs
  pkg  <- readDotCabal (head args)
  print . maintainer . packageDescription $ pkg
