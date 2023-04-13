{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_microlens (
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
version = Version [0,4,13,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/beggers/.cabal/store/ghc-9.4.4/mcrlns-0.4.13.1-e4baf2e0/bin"
libdir     = "/Users/beggers/.cabal/store/ghc-9.4.4/mcrlns-0.4.13.1-e4baf2e0/lib"
dynlibdir  = "/Users/beggers/.cabal/store/ghc-9.4.4/lib"
datadir    = "/Users/beggers/.cabal/store/ghc-9.4.4/mcrlns-0.4.13.1-e4baf2e0/share"
libexecdir = "/Users/beggers/.cabal/store/ghc-9.4.4/mcrlns-0.4.13.1-e4baf2e0/libexec"
sysconfdir = "/Users/beggers/.cabal/store/ghc-9.4.4/mcrlns-0.4.13.1-e4baf2e0/etc"

getBinDir     = catchIO (getEnv "microlens_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "microlens_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "microlens_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "microlens_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "microlens_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "microlens_sysconfdir") (\_ -> return sysconfdir)




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
