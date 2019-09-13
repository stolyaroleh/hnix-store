module System.Nix.Store.Remote.Util where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (ask, liftIO)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy      as LBS

import           Network.Socket.ByteString (recv, sendAll)

import           System.Nix.Store.Remote.Types

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- ask
  liftIO $ sendAll soc $ LBS.toStrict $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = go . runGetIncremental
  where
    go :: Decoder a -> MonadStore a
    go (Done _leftover _consumed x) = return x
    go (Partial cont) = do
      sock <- ask
      chunk <- liftIO (recv sock 8)
      go (cont (Just chunk))
    go (Fail _leftover _consumed msg) =
      throwError (ParseError msg)
