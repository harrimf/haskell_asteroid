{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_gloss_counter (
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

bindir     = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\bin"
libdir     = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\lib\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0-AZJn2Fw613G5GpcUNu5vY2-gloss-counter"
dynlibdir  = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\share\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0"
libexecdir = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\libexec\\x86_64-windows-ghc-8.6.5\\gloss-counter-0.1.0.0"
sysconfdir = "C:\\haskell_development\\game\\gloss-counter\\.stack-work\\install\\2015b563\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gloss_counter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gloss_counter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gloss_counter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gloss_counter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_counter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_counter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
