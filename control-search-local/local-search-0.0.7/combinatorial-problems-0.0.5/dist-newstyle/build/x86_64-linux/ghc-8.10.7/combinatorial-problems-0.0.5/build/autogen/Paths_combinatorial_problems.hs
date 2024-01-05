{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_combinatorial_problems (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/vlad/.cabal/bin"
libdir     = "/home/vlad/.cabal/lib/x86_64-linux-ghc-8.10.7/combinatorial-problems-0.0.5-inplace"
dynlibdir  = "/home/vlad/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/vlad/.cabal/share/x86_64-linux-ghc-8.10.7/combinatorial-problems-0.0.5"
libexecdir = "/home/vlad/.cabal/libexec/x86_64-linux-ghc-8.10.7/combinatorial-problems-0.0.5"
sysconfdir = "/home/vlad/.cabal/etc"

getBinDir     = catchIO (getEnv "combinatorial_problems_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "combinatorial_problems_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "combinatorial_problems_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "combinatorial_problems_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "combinatorial_problems_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "combinatorial_problems_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
