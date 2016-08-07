module Paths_Organon (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/luiz/Desktop/Organon/.cabal-sandbox/bin"
libdir     = "/home/luiz/Desktop/Organon/.cabal-sandbox/lib/i386-linux-ghc-7.8.3/Organon-0.1.0.0"
datadir    = "/home/luiz/Desktop/Organon/.cabal-sandbox/share/i386-linux-ghc-7.8.3/Organon-0.1.0.0"
libexecdir = "/home/luiz/Desktop/Organon/.cabal-sandbox/libexec"
sysconfdir = "/home/luiz/Desktop/Organon/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Organon_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Organon_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Organon_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Organon_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Organon_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
