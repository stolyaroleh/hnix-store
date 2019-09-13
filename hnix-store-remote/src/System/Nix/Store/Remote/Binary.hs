{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module System.Nix.Store.Remote.Binary where


import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word (Word64)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           System.Nix.StorePathMetadata
import           System.Nix.Store.Remote.Types

getMany :: Get a -> Get [a]
getMany parser = do
  count <- getInt
  replicateM count parser

getInt :: Integral a => Get a
getInt = fromIntegral <$> getWord64le

putInt :: Integral a => a -> Put
putInt = putWord64le . fromIntegral

getBool :: Get Bool
getBool = do
  x <- getInt @Int
  case x of
    0 -> return False
    1 -> return True
    _ -> fail $ "Unexpected boolean value (" ++ show x ++ ")"

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)

-- length prefixed string packing with padding to 8 bytes
putByteStringLen :: ByteString -> Put
putByteStringLen str = do
  putInt $ len
  putByteString str
  pad $ 8 - (len `mod` 8)
  where len = BS.length str
        pad x = replicateM_ x (putWord8 0)

putByteStrings :: Foldable t => t ByteString -> Put
putByteStrings xs = do
  putInt $ length xs
  mapM_ putByteStringLen xs

getByteStringLen :: Get ByteString
getByteStringLen = do
  len <- getInt
  st <- getByteString len
  when (len `mod` 8 /= 0) $ do
    pads <- unpad $ fromIntegral $ 8 - (len `mod` 8)
    unless (all (==0) pads) $ fail $ "No zeroes" ++ show (st, len, pads)
  return st
  where unpad x = replicateM x getWord8

getByteStrings :: Get [ByteString]
getByteStrings = getMany getByteStringLen

getTime :: Get UTCTime
getTime = do
  seconds <- getInt @Word64
  return (posixSecondsToUTCTime (fromIntegral seconds))

getTrust :: Get StorePathTrust
getTrust = do
  trust' <- getBool
  if trust'
    then return BuiltLocally
    else return BuiltElsewhere

putBuildMode :: BuildMode -> Put
putBuildMode mode = putInthost $
  case mode of
    Normal -> 0
    Repair -> 1
    Check -> 2

putVerbosity :: Verbosity -> Put
putVerbosity v = putInthost $
  case v of
    LvlError -> 0
    LvlInfo -> 1
    LvlTalkative -> 2
    LvlChatty -> 3
    LvlDebug -> 4
    LvlVomit -> 5

getVerbosity :: Get Verbosity
getVerbosity = do
  x <- getInt @Int
  case x of
    0 -> return LvlError
    1 -> return LvlInfo
    2 -> return LvlTalkative
    3 -> return LvlChatty
    4 -> return LvlDebug
    5 -> return LvlVomit
    _ -> fail $ "Unexpected verbosity value (" ++ show x ++ ")"

putActivityType :: ActivityType -> Put
putActivityType t = putInthost $
  case t of
    ActUnknown -> 0
    ActCopyPath -> 100
    ActDownload -> 101
    ActRealise -> 102
    ActCopyPaths -> 103
    ActBuilds -> 104
    ActBuild -> 105
    ActOptimiseStore -> 106
    ActVerifyPaths -> 107
    ActSubstitute -> 108
    ActQueryPathInfo -> 109
    ActPostBuildHook -> 110

getActivityType :: Get ActivityType
getActivityType = do
  x <- getInt @Int
  case x of
    0 -> return ActUnknown
    100 -> return ActCopyPath
    101 -> return ActDownload
    102 -> return ActRealise
    103 -> return ActCopyPaths
    104 -> return ActBuilds
    105 -> return ActBuild
    106 -> return ActOptimiseStore
    107 -> return ActVerifyPaths
    108 -> return ActSubstitute
    109 -> return ActQueryPathInfo
    110 -> return ActPostBuildHook
    _ -> fail $ "Unexpected ActivityType value (" ++ show x ++ ")"

putResultType :: ResultType -> Put
putResultType t = putInthost $
  case t of
    ResFileLinked -> 100
    ResBuildLogLine -> 101
    ResUntrustedPath -> 102
    ResCorruptedPath -> 103
    ResSetPhase -> 104
    ResProgress -> 105
    ResSetExpected -> 106
    ResPostBuildLogLine -> 107

getResultType :: Get ResultType
getResultType = do
  x <- getInt @Int
  case x of
    100 -> return ResFileLinked
    101 -> return ResBuildLogLine
    102 -> return ResUntrustedPath
    103 -> return ResCorruptedPath
    104 -> return ResSetPhase
    105 -> return ResProgress
    106 -> return ResSetExpected
    107 -> return ResPostBuildLogLine
    _ -> fail $ "Unexpected ResultType value (" ++ show x ++ ")"

getSubstitutablePathInfo :: Get SubstitutablePathInfo
getSubstitutablePathInfo = do
  deriver' <- getByteStringLen
  references' <- getByteStrings
  downloadSize' <- getInt @Word64
  narSize' <- getInt @Word64
  return $ SubstitutablePathInfo
    { deriver = deriver'
    , references = references'
    , downloadSize =
        if downloadSize' == 0
          then Nothing
          else Just (fromIntegral downloadSize')
    , narSize =
        if narSize' == 0
          then Nothing
          else Just (fromIntegral narSize')
    }

getValidPathInfo :: ByteString -> Get ValidPathInfo
getValidPathInfo path' = do
  deriver' <- getByteStringLen
  narHash' <- getByteStringLen
  references' <- getByteStrings
  registrationTime' <- getTime
  narSize' <- getInt @Word64
  trust' <- getTrust
  sigs' <- getByteStrings
  ca' <- getByteStringLen
  return $ ValidPathInfo
    { path = path'
    , deriver = deriver'
    , narHash = narHash'
    , references = references'
    , registrationTime = registrationTime'
    , narSize =
      if narSize' == 0
        then Nothing
        else Just (fromIntegral narSize')
    , trust = trust'
    , sigs = sigs'
    , contentAddressableAddress =
      if ca' == ""
        then Nothing
        else Just ca'
    }

getGCRoot :: Get GCRoot
getGCRoot = GCRoot <$> getByteStringLen <*> getByteStringLen

putGCAction :: GCAction -> Put
putGCAction action = putInt @Int $ case action of
  ReturnLive -> 0
  ReturnDead -> 1
  DeleteDead -> 2
  DeleteSpecific _ -> 3

getGCResult :: Get GCResult
getGCResult = GCResult <$> getByteStrings <*> getInt <* getInt @Int
