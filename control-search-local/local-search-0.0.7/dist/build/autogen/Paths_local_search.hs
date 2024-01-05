module Paths_local_search (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,7], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/richard/.cabal/bin"
libdir     = "/home/richard/.cabal/lib/local-search-0.0.7/ghc-7.4.2"
datadir    = "/home/richard/.cabal/share/local-search-0.0.7"
libexecdir = "/home/richard/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "local_search_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "local_search_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "local_search_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "local_search_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
