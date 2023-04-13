{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_monad_loops (
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
version = Version [0,4,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/beggers/.cabal/store/ghc-9.4.4/mnd-lps-0.4.3-a99db59f/bin"
libdir     = "/Users/beggers/.cabal/store/ghc-9.4.4/mnd-lps-0.4.3-a99db59f/lib"
dynlibdir  = "/Users/beggers/.cabal/store/ghc-9.4.4/lib"
datadir    = "/Users/beggers/.cabal/store/ghc-9.4.4/mnd-lps-0.4.3-a99db59f/share"
libexecdir = "/Users/beggers/.cabal/store/ghc-9.4.4/mnd-lps-0.4.3-a99db59f/libexec"
sysconfdir = "/Users/beggers/.cabal/store/ghc-9.4.4/mnd-lps-0.4.3-a99db59f/etc"

getBinDir     = catchIO (getEnv "monad_loops_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "monad_loops_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "monad_loops_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "monad_loops_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monad_loops_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monad_loops_sysconfdir") (\_ -> return sysconfdir)




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
