{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module API.RBPCP.RBPCP where

import           Internal.Types
import           API.RBPCP.Types
import           Data.Bitcoin.PaymentChannel.Types (BitcoinLockTime, FullPayment, SendPubKey)
import           Servant.API
import           Data.Word (Word32)


type VER = "v2"


-- Example: /funding/028adc96575e3ee23a69eb17723911e77c5c06320e4354b1518bb635f32793c910/1474949961/info
type FundInfo  = VER :> "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "info"                                   :> Get '[JSON] FundingInfo
type BeginOpen = VER :> "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "begin_open" :> Header "Host" String     :> Get '[JSON] ChannelLocation
-- Example: /channels/028adc96575e3ee23a69eb17723911e77c5c06320e4354b1518bb635f32793c910/1474949961/53ee3615ac0dd479ec1d3e144eb651f65764d3a5e400c04cf3c79425e8b22fb0/2
type ChanOpen  = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" TxHash   :> Capture "funding_vout" Word32                   :> ReqBody '[JSON] FullPayment      :> Verb 'POST 201 '[JSON] PaymentResult
type ChanPay   = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" TxHash   :> Capture "funding_vout" Word32                   :> ReqBody '[JSON] FullPayment      :> Put '[JSON] PaymentResult
type ChanClose = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" TxHash   :> Capture "funding_vout" Word32     :> "close"    :> ReqBody '[JSON] FullPayment      :> Put '[JSON] PaymentResult


-- | RESTful Bitcoin payment channel protocol
type RBPCP =
       FundInfo
  :<|> BeginOpen
  :<|> ChanOpen
  :<|> ChanPay
  :<|> ChanClose



-- Short-hands
type BLT = BitcoinLockTime