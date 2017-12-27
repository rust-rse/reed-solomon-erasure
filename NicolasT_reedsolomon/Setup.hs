{-# LANGUAGE RecordWildCards #-}

import Prelude hiding ((<$>))

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isJust)

import System.Directory hiding (makeAbsolute, withCurrentDirectory)
import System.FilePath

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Builtin (arProgram, ghcProgram)
import qualified Distribution.Simple.Program.Db as Db
import Distribution.Simple.Program.Types (ConfiguredProgram(..), Program(..), ProgramLocation(..))
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

main :: IO ()
main = defaultMainWithHooks customHooks
  where
    customHooks = simpleUserHooks { confHook = customConfHook (confHook simpleUserHooks)
                                  , buildHook = customBuildHook (buildHook simpleUserHooks)
                                  , preSDist = customPreSDist (preSDist simpleUserHooks)
                                  }

customConfHook :: (a -> b -> IO LocalBuildInfo)
               -> a
               -> b
               -> IO LocalBuildInfo
customConfHook innerHook a b = do
    localBuildInfo <- innerHook a b

    root <- makeAbsolute =<< getCurrentDirectory

    let cabalBuildDir = buildDir localBuildInfo
        sh = "sh"

        Just ar = Db.lookupProgram arProgram (withPrograms localBuildInfo)
        arLocation = resolveProgramLocation ar
        arWrapperPath = root </> "build-tools" </> "ar-wrapper"
        arWrapper = ar { programDefaultArgs = arWrapperPath : arLocation : programDefaultArgs ar
                       , programLocation = UserSpecified sh
                       , programOverrideEnv = ("CABAL_BUILD_DIR", Just cabalBuildDir) : programOverrideEnv ar
                       }

        Just ghc = Db.lookupProgram ghcProgram (withPrograms localBuildInfo)
        ghcLocation = resolveProgramLocation ghc
        ghcWrapperPath = root </> "build-tools" </> "ghc-wrapper"
        ghcWrapper = ghc { programDefaultArgs = ghcWrapperPath : ghcLocation : programDefaultArgs ghc
                         , programLocation = UserSpecified sh
                         , programOverrideEnv = ("CABAL_BUILD_DIR", Just cabalBuildDir) : programOverrideEnv ghc
                         }

        withPrograms' = Db.updateProgram arWrapper $
                            Db.updateProgram ghcWrapper $
                            withPrograms localBuildInfo

    return localBuildInfo { withPrograms = withPrograms' }
  where
    resolveProgramLocation prog = case programLocation prog of
                                    UserSpecified path -> path
                                    FoundOnSystem path -> path

customBuildHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ())
                -> PackageDescription
                -> LocalBuildInfo
                -> UserHooks
                -> BuildFlags
                -> IO ()
customBuildHook innerHook packageDescription@PackageDescription{..} localBuildInfo@LocalBuildInfo{..} userHooks buildFlags = do
    let wantSIMD = fromMaybe True $ lookup (FlagName "simd") $ configConfigurationsFlags configFlags
        verbosity = fromFlag $ buildVerbosity buildFlags
        version = pkgVersion package

    root <- makeAbsolute =<< getCurrentDirectory
    absBuildDir <- makeAbsolute buildDir
    let cbitsBuildDir = absBuildDir </> "cbits"

    when wantSIMD $ do
        let configure = root </> "cbits" </> "configure"

        configureExists <- doesFileExist configure
        unless configureExists $
            die $ concat [ "'configure' script not found. "
                         , "If this is not a release version, you probably need "
                         , "to run 'autoreconf -i' in the 'cbits' directory to generate it."
                         ]

        createDirectoryIfMissing True cbitsBuildDir

        hasMakefile <- doesFileExist (cbitsBuildDir </> "Makefile")
        unless hasMakefile $
            withCurrentDirectory cbitsBuildDir $ do
                let libOptions = case versionBranch version of
                        [999] -> []
                        _ -> ["--disable-maintainer-mode"]
                    fromWindows = map (\c -> if c == '\\' then '/' else c)
                rawSystemExit verbosity "sh" $ [ fromWindows configure
                                               ] ++ libOptions

        rawSystemExit verbosity "make" ["-C", cbitsBuildDir, "--no-print-directory"]

    let addIncludeDir info = info { includeDirs = cbitsBuildDir : includeDirs info }
        packageDescription' = if wantSIMD
                                then mapBuildInfo addIncludeDir packageDescription
                                else packageDescription

    innerHook packageDescription' localBuildInfo userHooks buildFlags

mapBuildInfo :: (BuildInfo -> BuildInfo) -> PackageDescription -> PackageDescription
mapBuildInfo f desc = desc { library = fmap (\l -> l { libBuildInfo = f (libBuildInfo l) }) (library desc)
                           , executables = map (\e -> e { buildInfo = f (buildInfo e) }) (executables desc)
                           , testSuites = map (\t -> t { testBuildInfo = f (testBuildInfo t) }) (testSuites desc)
                           , benchmarks = map (\b -> b { benchmarkBuildInfo = f (benchmarkBuildInfo b) }) (benchmarks desc)
                           }

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = (normalise <$>) . absolutize
  where absolutize path -- avoid the call to `getCurrentDirectory` if we can
          | isRelative path = (</> path) . addTrailingPathSeparator <$>
                              getCurrentDirectory
          | otherwise       = return path

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

customPreSDist :: (Args -> SDistFlags -> IO HookedBuildInfo)
               -> Args
               -> SDistFlags
               -> IO HookedBuildInfo
customPreSDist innerHook args flags = do
    hookedBuildInfo <- innerHook args flags

    root <- makeAbsolute =<< getCurrentDirectory
    let cbits = root </> "cbits"
        verbosity = fromFlag (sDistVerbosity flags)

    withCurrentDirectory cbits $
        rawSystemExit verbosity "autoreconf" ["-f", "-i"]

    return hookedBuildInfo
