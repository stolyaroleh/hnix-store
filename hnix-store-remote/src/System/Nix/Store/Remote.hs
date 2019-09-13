{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module System.Nix.Store.Remote (
    BuildMode(..)
  , runStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  , buildPaths
  ) where

import           Data.ByteString (ByteString)
import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol

type RepairFlag = Bool
type CheckFlag = Bool

syncWithGC :: MonadStore ()
syncWithGC = runOp_ SyncWithGC

optimiseStore :: MonadStore ()
optimiseStore = runOp_ OptimiseStore

verifyStore :: CheckFlag -> RepairFlag -> MonadStore ()
verifyStore check repair = runOpArgs_ VerifyStore $ do
  putBool check
  putBool repair

buildPaths ::
  -- forall storeDir . (KnownStoreDir storeDir) =>
  -- [StorePath storeDir]
  [ByteString] -> BuildMode -> MonadStore ()
buildPaths drvs mode =
  runOpArgs_ BuildPaths args
  where
    args = do
      putByteStrings drvs
      putBuildMode mode
