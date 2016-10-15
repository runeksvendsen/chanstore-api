{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
module ChanStore.Types.Request
(
    module ChanStore.Types.Request
  , module BTC

)

 where

import qualified Data.Bitcoin.PaymentChannel.Types as BTC
import qualified Data.Bitcoin.PaymentChannel.Util as Util
import qualified RBPCP.Types as RBPCP
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as Bin
import           Data.String.Conversions (cs)
import qualified Servant.API.ContentTypes as Content

import           GHC.Generics
import           Data.Typeable


type Key = BTC.SendPubKey
type RPC = BTC.ReceiverPaymentChannel

data FetchError = DoesntExist

data ChannelResource = ChannelResource BTC.SendPubKey BTC.BitcoinLockTime HT.OutPoint
    deriving Generic

data GetRequest = GetRequest Key deriving (Show, Generic)
data GetResult =  GetResult BTC.ReceiverPaymentChannel
    deriving Generic

data CreateRequest = CreateRequest RPC deriving (Show, Generic)
data CreateResult =
    ChannelCreated
  | ChannelExists
        deriving Generic

data UpdateRequest = UpdateRequest RPC deriving Generic
data UpdateResult =
    ChannelUpdates
  | UpdateNotInProgress
        deriving Generic

data CloseBeginRequest = CloseBeginRequest ChannelResource BTC.FullPayment deriving Generic
data CloseBeginResult  =
    CloseInitiated      (RPC,BTC.BitcoinAmount)
  | ClosingPaymentError    BTC.PayChanError
  | CloseUpdateError    UpdateResult
        deriving Generic

-- |Management
-- data ChanInfoRequest = DataPayloadRequest Key JSONString BitcoinAmount deriving Generic
data ChanInfoResult =
    OK RPC RBPCP.ChannelStatus (Maybe HT.TxHash)
  | ChanNotFound
        deriving Generic

instance Bin.Serialize ChannelResource
instance Bin.Serialize GetRequest
instance Bin.Serialize GetResult
instance Bin.Serialize CreateRequest
instance Bin.Serialize CreateResult
instance Bin.Serialize UpdateRequest
instance Bin.Serialize UpdateResult
instance Bin.Serialize CloseBeginRequest
instance Bin.Serialize CloseBeginResult
instance Bin.Serialize ChanInfoResult

class (Bin.Serialize a, Typeable a) => ConvertBin a where
    deser :: BL.ByteString -> Either String a
    deser = Util.deserEither . BL.toStrict
    ser :: a -> BL.ByteString
    ser = BL.fromStrict . Bin.encode

instance ConvertBin GetRequest
instance ConvertBin GetResult
instance ConvertBin CreateRequest
instance ConvertBin CreateResult
instance ConvertBin UpdateRequest
instance ConvertBin UpdateResult
instance ConvertBin CloseBeginRequest
instance ConvertBin CloseBeginResult
instance ConvertBin ChanInfoResult

-- Orphans
instance Content.MimeUnrender Content.OctetStream GetRequest where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream GetRequest where mimeRender _ = ser
instance Content.MimeUnrender Content.OctetStream GetResult where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream GetResult where mimeRender _ = ser

instance Content.MimeUnrender Content.OctetStream CreateRequest where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream CreateRequest where mimeRender _ = ser
instance Content.MimeUnrender Content.OctetStream CreateResult where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream CreateResult where mimeRender _ = ser

instance Content.MimeUnrender Content.OctetStream UpdateRequest where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream UpdateRequest where mimeRender _ = ser
instance Content.MimeUnrender Content.OctetStream UpdateResult where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream UpdateResult where mimeRender _ = ser

instance Content.MimeUnrender Content.OctetStream CloseBeginRequest where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream CloseBeginRequest where mimeRender _ = ser
instance Content.MimeUnrender Content.OctetStream CloseBeginResult where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream CloseBeginResult where mimeRender _ = ser

instance Content.MimeUnrender Content.OctetStream ChanInfoResult where mimeUnrender _ = deser
instance Content.MimeRender Content.OctetStream ChanInfoResult where mimeRender _ = ser


