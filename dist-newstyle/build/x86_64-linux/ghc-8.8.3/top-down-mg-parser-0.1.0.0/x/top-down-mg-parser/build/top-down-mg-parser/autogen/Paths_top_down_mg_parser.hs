{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_top_down_mg_parser (
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/juliangrove/.cabal/bin"
libdir     = "/home/juliangrove/.cabal/lib/x86_64-linux-ghc-8.8.3/top-down-mg-parser-0.1.0.0-inplace-top-down-mg-parser"
dynlibdir  = "/home/juliangrove/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/juliangrove/.cabal/share/x86_64-linux-ghc-8.8.3/top-down-mg-parser-0.1.0.0"
libexecdir = "/home/juliangrove/.cabal/libexec/x86_64-linux-ghc-8.8.3/top-down-mg-parser-0.1.0.0"
sysconfdir = "/home/juliangrove/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "top_down_mg_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "top_down_mg_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "top_down_mg_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "top_down_mg_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "top_down_mg_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "top_down_mg_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
