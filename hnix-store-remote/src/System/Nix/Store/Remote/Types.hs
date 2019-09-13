{-# LANGUAGE DuplicateRecordFields #-}
module System.Nix.Store.Remote.Types (
    MonadStore
  , Verbosity(..)
  , ActivityType(..)
  , ResultType(..)
  , Error(..)
  , Logger(..)
  , Field(..)
  , BuildMode(..)
  , ValidPathInfo(..)
  , SubstitutablePathInfo(..)
  , GCRoot(..)
  , GCAction(..)
  , GCResult(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString           (ByteString)
import           Data.Time                 (UTCTime)
import           Network.Socket            (Socket)
import           Pipes
import           System.Nix.StorePathMetadata (StorePathTrust)

data Error =
    LogError Int ByteString
  | ParseError String
  | ConnError String
  deriving (Eq, Show)

type MonadStore a = ExceptT Error (Producer Logger (ReaderT Socket IO)) a

type ActivityID = Int
type ActivityParentID = Int

data ActivityType =
    ActUnknown
  | ActCopyPath
  | ActDownload
  | ActRealise
  | ActCopyPaths
  | ActBuilds
  | ActBuild
  | ActOptimiseStore
  | ActVerifyPaths
  | ActSubstitute
  | ActQueryPathInfo
  | ActPostBuildHook
  deriving (Eq, Ord, Show)

data ResultType =
    ResFileLinked
  | ResBuildLogLine
  | ResUntrustedPath
  | ResCorruptedPath
  | ResSetPhase
  | ResProgress
  | ResSetExpected
  | ResPostBuildLogLine
  deriving (Eq, Ord, Show)

data Verbosity =
    LvlError
  | LvlInfo
  | LvlTalkative
  | LvlChatty
  | LvlDebug
  | LvlVomit
  deriving (Eq, Ord, Show)

data Field = LogStr ByteString | LogInt Int
  deriving (Eq, Ord, Show)

data Logger =
    Next          ByteString
  | Read          Int        -- data needed from source
  | Write         ByteString -- data for sink
  | Last
  | Error         Int ByteString
  | StartActivity ActivityID Verbosity ActivityType ByteString [Field] ActivityParentID
  | StopActivity  ActivityID
  | Result        ActivityID ResultType [Field]
  deriving (Eq, Ord, Show)

data BuildMode = Normal | Repair | Check
  deriving (Eq, Show)

data ValidPathInfo = ValidPathInfo
  { path :: ByteString
  , deriver :: ByteString
  , narHash :: ByteString
  , references :: [ByteString]
  , registrationTime :: UTCTime
  , narSize :: Maybe Integer
  , trust :: StorePathTrust
  , sigs :: [ByteString]
  , contentAddressableAddress :: Maybe ByteString
  }
  deriving (Eq, Show)

data SubstitutablePathInfo = SubstitutablePathInfo
  { deriver :: ByteString
  , references :: [ByteString]
  , downloadSize :: Maybe Integer
  , narSize :: Maybe Integer
  }
  deriving (Eq, Ord, Show)

data GCRoot = GCRoot
  { root :: ByteString
  , path :: ByteString
  }
  deriving (Eq, Show)

data GCAction = ReturnLive | ReturnDead | DeleteDead | DeleteSpecific [ByteString]
  deriving (Eq, Show)

data GCResult = GCResult
  { paths :: [ByteString]
  , bytesFreed :: Integer
  }
  deriving (Eq, Show)
