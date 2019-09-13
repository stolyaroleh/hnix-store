{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module System.Nix.Store.Remote (
    BuildMode(..)
  , SubstitutablePathInfo(..)
  , runStore
  , isValidPath
  , hasSubstitutes
  , addTextToStore
  , buildPaths
  , ensurePath
  , addTempRoot
  , addIndirectRoot
  , findRoots
  , collectGarbage
  , querySubstitutablePathInfo
  , queryDerivationOutputs
  , queryAllValidPaths
  , queryPathInfo
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryValidPaths
  , querySubstitutablePaths
  , queryValidDerivers
  , queryReferrers
  , syncWithGC
  , optimiseStore
  , verifyStore
  , querySubstitutablePathInfos
  ) where

import           Control.Monad (replicateM_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.ByteString (ByteString)
import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Types

type StorePath = ByteString

isValidPath :: StorePath -> MonadStore Bool
isValidPath path' = runOpArgs IsValidPath (putByteStringLen path') getBool

hasSubstitutes :: StorePath -> MonadStore Bool
hasSubstitutes path' = runOpArgs HasSubstitutes (putByteStringLen path') getBool

type RecursiveFlag = Bool

addTextToStore :: ByteString -> StorePath -> [StorePath] -> MonadStore StorePath
addTextToStore suffix contents refs = runOpArgs AddTextToStore putArgs getResult
  where
    putArgs = do
      putByteStringLen suffix
      putByteStringLen contents
      putByteStrings refs
    getResult = getByteStringLen

buildPaths :: [StorePath] -> BuildMode -> MonadStore ()
buildPaths drvs mode =
  runOpArgs_ BuildPaths args
  where
    args = do
      putByteStrings drvs
      putBuildMode mode

ensurePath :: StorePath -> MonadStore ()
ensurePath path' = runOpArgs_ EnsurePath (putByteStringLen path')

addTempRoot :: StorePath -> MonadStore ()
addTempRoot path' = runOpArgs_ AddTempRoot (putByteStringLen path')

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot path' = runOpArgs_ AddIndirectRoot (putByteStringLen path')

findRoots :: MonadStore [GCRoot]
findRoots = runOpArgs FindRoots mempty (getMany getGCRoot)

type IgnoreLivenessFlag = Bool

collectGarbage :: GCAction -> IgnoreLivenessFlag -> Int -> MonadStore GCResult
collectGarbage action ignoreLiveness maxFreed = runOpArgs CollectGarbage putArgs getGCResult
  where
    putArgs = do
      putGCAction action
      case action of
        DeleteSpecific paths' -> putByteStrings paths'
        _ -> putByteStrings []
      putBool ignoreLiveness
      putInt maxFreed

      -- Obsolete fields
      replicateM_ 3 (putInt @Int 0)

querySubstitutablePathInfo :: StorePath -> MonadStore (Maybe SubstitutablePathInfo)
querySubstitutablePathInfo path' = runOpArgs QuerySubstitutablePathInfo (putByteStringLen path') getResults
  where
    getResults = do
      ok <- getBool
      if ok
        then Just <$> getSubstitutablePathInfo
        else return Nothing

queryDerivationOutputs :: StorePath -> MonadStore [ByteString]
queryDerivationOutputs drv = runOpArgs QueryDerivationOutputs putArgs getResult
  where
    putArgs = putByteStringLen drv
    getResult = getMany getByteStringLen

queryAllValidPaths :: MonadStore [StorePath]
queryAllValidPaths = runOp QueryAllValidPaths getByteStrings

queryPathInfo :: StorePath -> MonadStore (Maybe ValidPathInfo)
queryPathInfo path' = runOpArgs QueryPathInfo putArgs getResult
  where
    putArgs = putByteStringLen path'
    getResult = do
      x <- getInt @Int
      if x == 1
        then Just <$> getValidPathInfo path'
        else return Nothing

queryDerivationOutputNames :: StorePath -> MonadStore [ByteString]
queryDerivationOutputNames drv = runOpArgs QueryDerivationOutputNames putArgs getResult
  where
    putArgs = putByteStringLen drv
    getResult = getMany getByteStringLen

queryPathFromHashPart :: ByteString -> MonadStore StorePath
queryPathFromHashPart hashPart = runOpArgs QueryPathFromHashPart (putByteStringLen hashPart) getByteStringLen

querySubstitutablePathInfos :: [StorePath] -> MonadStore (Map StorePath SubstitutablePathInfo)
querySubstitutablePathInfos paths' = runOpArgs QuerySubstitutablePathInfos putArgs getResult
  where
    putArgs = putByteStrings paths'
    getResult = do
      items <- getMany ((,) <$> getByteStringLen <*> getSubstitutablePathInfo)
      return (Map.fromList items)

queryValidPaths :: [StorePath] -> MonadStore (Set StorePath)
queryValidPaths paths' = runOpArgs QueryValidPaths putArgs getResults
  where
    putArgs = putByteStrings paths'
    getResults = Set.fromList <$> getByteStrings

querySubstitutablePaths :: [StorePath] -> MonadStore (Set StorePath)
querySubstitutablePaths paths' = runOpArgs QuerySubstitutablePaths putArgs getResults
  where
    putArgs = putByteStrings paths'
    getResults = Set.fromList <$> getByteStrings

queryValidDerivers :: StorePath -> MonadStore [ByteString]
queryValidDerivers path' = runOpArgs QueryValidDerivers (putByteStringLen path') getByteStrings

optimiseStore :: MonadStore ()
optimiseStore = runOp_ OptimiseStore

type RepairFlag = Bool
type CheckFlag = Bool

verifyStore :: CheckFlag -> RepairFlag -> MonadStore ()
verifyStore check repair = runOpArgs_ VerifyStore $ do
  putBool check
  putBool repair

syncWithGC :: MonadStore ()
syncWithGC = runOp_ SyncWithGC

queryReferrers :: StorePath -> MonadStore [ByteString]
queryReferrers path' = runOpArgs QueryReferrers (putByteStringLen path') getByteStrings
