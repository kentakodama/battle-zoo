{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_battle_zoo (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/kkodama/.cabal/bin"
libdir     = "/Users/kkodama/.cabal/lib/x86_64-osx-ghc-8.10.7/battle-zoo-0.1.0.0-inplace-battle-zoo"
dynlibdir  = "/Users/kkodama/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/kkodama/.cabal/share/x86_64-osx-ghc-8.10.7/battle-zoo-0.1.0.0"
libexecdir = "/Users/kkodama/.cabal/libexec/x86_64-osx-ghc-8.10.7/battle-zoo-0.1.0.0"
sysconfdir = "/Users/kkodama/.cabal/etc"

getBinDir     = catchIO (getEnv "battle_zoo_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "battle_zoo_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "battle_zoo_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "battle_zoo_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "battle_zoo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "battle_zoo_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
