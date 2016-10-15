{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module ChanStore.API where

import           ChanStore.Types.Request
import           Data.Bitcoin.PaymentChannel.Types
import qualified Network.Haskoin.Transaction as HT

import           Servant.API
import           Data.Time.Clock (UTCTime)


-- |Get channel by sender pubkey
type StoreGet =
    "store"  :> "by_id"  :> Capture "client_pk" SendPubKey  :> Get  '[OctetStream] GetResult

-- |Create channel
type StoreCreate =
    "store"  :> "by_id"  :> Capture "client_pk" SendPubKey
        :> ReqBody '[OctetStream] CreateRequest             :> Post '[OctetStream] CreateResult

-- |Update channel with new ReceiverPaymentChannel
type StoreUpdate =
    "store"  :> "by_id"  :> Capture "client_pk" SendPubKey
        :> ReqBody '[OctetStream] UpdateRequest             :> Put  '[OctetStream] UpdateResult

-- |Get a list of all open payment channels
type StoreGetOpen =
    "store"  :> "all" :> "open"                             :> Get  '[OctetStream] [ReceiverPaymentChannel]



type ChanStore =
       StoreGet
  :<|> StoreCreate
  :<|> StoreUpdate
  :<|> StoreGetOpen

  -- Management
  :<|> "manage" :> "get"  :> Capture "client_pk" SendPubKey :> Get  '[OctetStream]  ChanInfoResult

  -- Settlement
  :<|> "settle" :> "begin"  :> "by_info"
            :> ReqBody '[OctetStream] CloseBeginRequest     :> Put  '[OctetStream]  CloseBeginResult

  :<|> "settle" :> "begin"  :> "by_exp"   :> Capture "exp" UTCTime
                                                            :> Put  '[OctetStream] [ReceiverPaymentChannel]

  :<|> "settle" :> "begin"  :> "by_value" :> Capture "val" BitcoinAmount
                                                            :> Put  '[OctetStream] [ReceiverPaymentChannel]

  :<|> "settle" :> "finish" :> "by_id"    :> Capture "client_pk" SendPubKey
            :> Capture "out" HT.TxHash
                                                            :> Post '[OctetStream]  NoContent

