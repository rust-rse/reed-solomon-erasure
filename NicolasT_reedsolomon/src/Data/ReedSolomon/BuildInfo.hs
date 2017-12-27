{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Data.ReedSolomon.BuildInfo
-- Description : Build information about the library
-- Copyright   : (C) 2015 Nicolas Trangez
-- License     : MIT (see the file LICENSE)
-- Maintainer  : Nicolas Trangez <ikke@nicolast.be>
-- Stability   : provisional
--
-- This module exposes data gathered while the library was being built,
-- including Git revision, dependency information, tool versions, Cabal
-- flags etc.

module Data.ReedSolomon.BuildInfo (
      buildInfo
    , BuildInfo(..)
    , DependencyInfo(..)
    , ToolInfo(..)
    , FlagInfo(..)
    , GitInfo(..)
    ) where

import qualified Development.GitRev as GR

-- | Git revision information
data GitInfo = GitInfo { gitRevision :: String  -- ^ Git revision hash
                       , gitBranch :: String  -- ^ Git branch
                       , gitDirty :: Bool  -- ^ State of the tree
                       }
  deriving (Show, Eq)

mkGitInfo :: Maybe GitInfo
mkGitInfo = case gitRevision info of
    "UNKNOWN" -> Nothing
    _ -> Just info
  where
    info = GitInfo { gitRevision = $(GR.gitHash)
                   , gitBranch = $(GR.gitBranch)
                   , gitDirty = $(GR.gitDirty)
                   }

-- | Cabal flag settings
data FlagInfo = FlagInfo { flagSIMD :: Bool  -- ^ SIMD flag was set
                         , flagLLVM :: Bool  -- ^ LLVM flag was set
                         }
  deriving (Show, Eq)

mkFlagInfo :: FlagInfo
mkFlagInfo = FlagInfo{..}
  where
#if HAVE_SIMD
    flagSIMD = True
#else
    flagSIMD = False
#endif

#if HAVE_LLVM
    flagLLVM = True
#else
    flagLLVM = False
#endif

-- | Build tool versions
--
-- Note: this doesn't include GHC version, which can be queried through
-- e.g. invocation with `+RTS --info`.
data ToolInfo = ToolInfo { toolGCC :: Maybe String  -- ^ GCC version
                         , toolLLVM :: Maybe Int  -- ^ LLVM version identifier
                         }
  deriving (Show, Eq)

mkToolInfo :: ToolInfo
mkToolInfo = ToolInfo{..}
  where
#ifdef TOOL_VERSION_gcc
    toolGCC = Just TOOL_VERSION_gcc
#else
    toolGCC = Nothing
#endif

#ifdef __GLASGOW_HASKELL_LLVM__
# if __GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1
    -- GHC 8.0.1 has a tuple as value of __GLASGOW_HASKELL_LLVM__, not
    -- a plain Int, due to some changes in 29310b62 and the usage of `show`
    -- Unless this becomes 'official API', treat it as a version-specific
    -- mistake
    toolLLVM = let (major, minor) = __GLASGOW_HASKELL_LLVM__ in Just (10 * major + minor)
# else
    toolLLVM = Just  __GLASGOW_HASKELL_LLVM__
# endif
#else
    toolLLVM = Nothing
#endif

-- | Dependency version information
data DependencyInfo = DependencyInfo { depBase :: String
                                     , depVector :: String
                                     , depLoop :: String
                                     , depPrimitive :: String
                                     , depMtl :: String
                                     , depExceptions :: String
                                     , depBytestring :: String
                                     , depProfunctors :: String
                                     }
  deriving (Show, Eq)

mkDependencyInfo :: DependencyInfo
mkDependencyInfo = DependencyInfo{..}
  where
    depBase = VERSION_base
    depVector = VERSION_vector
    depLoop = VERSION_loop
    depPrimitive = VERSION_primitive
    depMtl = VERSION_mtl
    depExceptions = VERSION_exceptions
    depBytestring = VERSION_bytestring
    depProfunctors = VERSION_profunctors

-- | Build information structure
data BuildInfo = BuildInfo { git :: Maybe GitInfo  -- ^ Git tree information, if built from Git
                           , flags :: FlagInfo  -- ^ Cabal flag settings
                           , tools :: ToolInfo  -- ^ Build tool versions
                           , dependencies :: DependencyInfo  -- ^ Dependency version information
                           }
  deriving (Show, Eq)

-- | Build information
buildInfo :: BuildInfo
buildInfo = BuildInfo{..}
  where
    git = mkGitInfo
    flags = mkFlagInfo
    tools = mkToolInfo
    dependencies = mkDependencyInfo
