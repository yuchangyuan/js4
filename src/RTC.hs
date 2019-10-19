{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# Language TupleSections #-}
{-# Language RecursiveDo #-}
{-# Language BlockArguments #-}

module RTC ( MqttWidget(..)
           , mqttWidget
           , MqttState(..)
           , rtcManagerNew
           , rtcManagerDummy
           , RTCManager(..)
           , consoleLog
           , rtcInit ) where

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.RTCPeerConnection as DOM
import qualified GHCJS.DOM.RTCIceCandidateEvent as DOME
import qualified GHCJS.DOM.RTCDataChannelEvent as DOME
import qualified GHCJS.DOM.RTCIceCandidate as DOM
import qualified GHCJS.DOM.RTCDataChannel as RTCDataChannel

import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.MessageEvent as DOM
import qualified GHCJS.DOM.Enums as Enums

import GHCJS.DOM.EventTargetClosures (unsafeEventName)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Language.Javascript.JSaddle( askJSM
                                  , JSM(..)
                                  , JSContextRef, JSException
                                  , syncPoint
                                  , runJSM
                                  , runJSaddle
                                  , JSVal(..)
                                  , ToJSVal, toJSVal
                                  , FromJSVal, fromJSVal, fromJSValUnchecked
                                  , toJSString
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2, jsg
                                  , new, obj
                                  , (<#)
                                  , maybeNullOrUndefined
                                  , strToText, textToStr
                                  , valToNumber, valIsNull, valToStr
                                  )
import Control.Lens ((^.))
import Control.Monad (when, unless, forM_, join)
--import Data.Aeson
import Data.List (stripPrefix, findIndex)
import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Either (fromLeft, fromRight)

import Control.Monad.IO.Class (liftIO, MonadIO(..))

import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe (isJust, isNothing)

import Reflex
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)

import Control.Monad.Catch (MonadCatch, catch)

cfg :: JSM JSVal
cfg = do
    urls <- obj
    (urls <# "urls") "stun:stun.stunprotocol.org"

    res <- obj
    (res <# "iceServers") [urls]

    toJSVal res

consoleLog :: ToJSVal a => a -> JSM ()
consoleLog a = do
  --x' <-jsg "JSON"  ^. js1 "stringify" x
  console <- jsg "console"
  console ^. js1 "log" a
  return ()


topicPrefix :: String
topicPrefix = "WebRTC/测试主题"

-- some hack, use js global variable
rtcInit :: JSM ()
rtcInit = do
   x <- obj
   jsg "window" ^. jss "_conn_map" x
   return ()

parseTopic :: String -> String -> Maybe (String, String, String)
parseTopic p s = do
   a <- stripPrefix p s
   i0 <- findIndex (== '/') a
   let b = take i0 a
   let c = drop (i0 + 1) a
   i1 <- findIndex (== '/') c
   let d = take i1 c
   let e = drop (i1 + 1) c
   return (b, d, e)

data SignalData = SignalData { signalLocalPeer :: Text --- local peer id
                             , signalRemotePeer :: Text -- remote peer id
                             , signalType :: Text -- offer, answer, candidate
                             , signalContent :: Text -- sdp or ice_candidate
                             }

sendSignal :: JSVal -> SignalData -> JSM ()
sendSignal client d = do
    --client <- mqttClient
    let topic = T.intercalate (T.singleton '/')
                              [ T.pack topicPrefix
                              , signalRemotePeer d
                              , signalLocalPeer d
                              , signalType d
                              ]
    let content = signalContent d

    paho <- jsg "Paho"
    msg <- new (paho ^. js "MQTT" ^. js "Message") content
    msg ^. jss "destinationName" topic

    catch (client ^. js1 "send" msg >> return ())
          (\(e :: JSException) -> do consoleLog ("js exception ", T.pack (show e))
                                     return ()
          )

    return ()


mqttProc :: (MqttState -> IO ()) -> (SignalData -> IO ()) ->
             Text -> JSM JSVal
mqttProc stT msgT localPeer = do
  liftIO $ stT MqttConnecting

  consoleLog ("local_peer: ", localPeer)
  console <- jsg "console"
  paho <- jsg "Paho"
  client <- new (paho ^. js "MQTT" ^. js "Client")
                ("test.mosquitto.org", 8081 :: Int, "/", "client" ++ T.unpack localPeer)
  client ^. jss "onMessageArrived" (fun $ \ _ _ [e] -> do
            payload <- strToText <$> (e ^. js "payloadString" >>= valToStr)
            topic <- strToText <$> (e ^. js "topic" >>= valToStr)
            case parseTopic (topicPrefix ++ "/") (T.unpack topic) of
                 Just (lid, rid, op) -> do
                      let dat = SignalData (T.pack lid) (T.pack rid) (T.pack op) payload
                      consoleLog (lid, rid, op, payload)
                      liftIO $ msgT dat
                 Nothing -> return ()
            )

  client ^. jss "onConnected" (fun $ \_ _ [rc, uri] -> do
      consoleLog ("connected:", uri)
      client ^. js1 "subscribe" (topicPrefix ++ "/" ++ T.unpack localPeer ++ "/#")
      liftIO $ stT MqttReady
      return ())

  opts <- obj
  opts ^. jss "useSSL" True
  opts ^. jss "reconnect" True

  client ^. jss "onConnectionLost" (fun $ \_ _ [ec, em] -> do
         liftIO $ stT MqttConnecting
         consoleLog ("lost connection", ec, em)

         --liftIO $ threadDelay $ 2000 * 1000
         --consoleLog "reconnect"
         --client ^. js1 "connect" opts
         return ())

  client ^. js1 "connect" opts

  -- for debug
  jsg "window" ^.jss "_mqtt_client" client

  return client


data MqttState = MqttReady | MqttConnecting | MqttClosed deriving (Show, Eq)

data MqttWidget t = MqttWidget { _mqtt_state :: Dynamic t MqttState
                               , _mqtt_message :: Event t SignalData
                               }

data RTCPeerData = RTCPeerData { _rtc_pc :: DOM.RTCPeerConnection
                               , _rtc_dc :: DOM.RTCDataChannel
                               }
type RTCPeerPData = Either DOM.RTCPeerConnection DOM.RTCDataChannel

findPeerPc :: Text -> Map Text RTCPeerData -> Map Text RTCPeerPData ->
              Maybe DOM.RTCPeerConnection
findPeerPc peer m1 m2 = let
  a = m1 M.!? peer
  b = m2 M.!? peer
  c = join $ ffor b $ either Just (const Nothing)
  in if isJust a then _rtc_pc <$> a else c

findPeerDc :: Text -> Map Text RTCPeerData -> Map Text RTCPeerPData ->
              Maybe DOM.RTCDataChannel
findPeerDc peer m1 m2 = let
  a = m1 M.!? peer
  b = m2 M.!? peer
  c = join $ ffor b $ either (const Nothing) Just
  in if isJust a then _rtc_dc <$> a else c

promiseH0 :: (MonadCatch m, DOM.MonadJSM m) => DOM.PromiseRejected -> m (Maybe a)
promiseH0 e = do
  DOM.liftJSM $ consoleLog ("promise rejected", DOM.rejectionReason e)
  return Nothing

procSignalData :: Text ->
                  Maybe DOM.RTCPeerConnection ->
                  Maybe DOM.RTCDataChannel ->
                  (SignalData -> JSM()) ->
                  (Text -> DOM.RTCDataChannel -> JSM()) ->
                  (Text -> DOM.RTCPeerConnection -> JSM()) ->
                  SignalData ->
                  JSM ()
procSignalData lp pc' dc' sendSignalFunc insertDCFunc insertPCFunc sd = do
  let tp = signalType sd
  let rp = signalRemotePeer sd
  let c  = signalContent sd

  -- TODO, parse may fail
  payload <- jsg "JSON" ^. js1 "parse" c

  case (T.unpack tp, pc') of
    ("offer", _) -> catch (do -- here, we ignore pc'
      pc <- connectPeer lp rp (Just payload) sendSignalFunc insertDCFunc
      liftIO $ insertPCFunc rp pc
      return ()
      ) (fmap (const ()) . promiseH0)
    ("candidate", Just pc) ->
      catch (DOM.addIceCandidate pc (DOM.RTCIceCandidate payload))
            (fmap (const ()) . promiseH0)
    ("answer", Just pc) ->
      catch (DOM.setRemoteDescription pc (DOM.RTCSessionDescriptionInit payload))
            (fmap (const ()) . promiseH0)
    _ -> return ()

-- (Text, Just xx) --> insert pc or dc
-- (Text, Nothing) --> remote pc/dc from peerDataMap
peerPDataFold :: (Text, Maybe RTCPeerPData) ->
                 (Map Text RTCPeerPData, Map Text RTCPeerData) ->
                 (Map Text RTCPeerPData, Map Text RTCPeerData)
peerPDataFold (peer, Nothing) (m1, m2) = (M.delete peer m1, M.delete peer m2)
peerPDataFold (peer, Just ppd) (m1, m2) =
  case (m1 M.!? peer, ppd) of
    (Nothing, _) ->
          (M.insert peer ppd m1, m2)
    (Just (Left _), Left _) ->
          (M.insert peer ppd m1, m2)
    (Just (Right _), Right _) ->
          (M.insert peer ppd m1, m2)

    (Just (Left pc),  Right dc) ->
          (M.delete peer m1, M.insert peer (RTCPeerData pc dc) m2)
    (Just (Right dc), Left pc) ->
          (M.delete peer m1, M.insert peer (RTCPeerData pc dc) m2)

pStFold :: (Text, Maybe Text) -> Map Text Text -> Map Text Text
pStFold (peer, Nothing) = M.delete peer
pStFold (peer, Just st) = M.insert peer st

data RTCManager t = RTCManager { _rtc_mqtt_state :: Dynamic t MqttState
                               , _rtc_peer_state :: Dynamic t (Map Text Text)
                               , _rtc_rx_msg :: Event t (Text, JSVal)
                               }

rtcManagerDummy :: (Reflex t) => RTCManager t
rtcManagerDummy = RTCManager (constDyn MqttClosed) (constDyn M.empty) never

rtcManagerNew :: ( Reflex t
              , MonadSample t m
              , MonadSample t (Performable m)
              , MonadFix m
              , DOM.MonadJSM m
              , MonadHold t m
              , PerformEvent t m
              , TriggerEvent t m
              , MonadIO (Performable m)
              ) =>
              Text -> -- local peer id
              Event t Text -> -- request remote peer id
              Event t (Text, Text) -> -- msgId, peerId, payload
              m (RTCManager t)
rtcManagerNew lp rpE txMsgE = mdo
  (txSdE, txSdT) <- newTriggerEvent
  mqtt <- mqttWidget lp txSdE

  (peerPDataE, peerPDataT) <- newTriggerEvent -- (peer, RTCPData)

  -- peerPMapE :: (Text, RTCPeerPData)
  (peerPDataMapD, peerDataMapD) <-
    splitDynPure <$> foldDyn peerPDataFold (M.empty, M.empty) peerPDataE

  (rxMsgE, rxMsgT) <- newTriggerEvent
  (pStE, pStT) <- newTriggerEvent -- use fold

  -- Text, Maybe Text
  pStD <- foldDyn pStFold M.empty pStE

  let stNeedClean x = (x == Just (T.pack "closed")) ||
                      (x == Just (T.pack "disconnected")) ||
                      (x == Just (T.pack "failed"))

  -- use pStE to check "disconnected" or "closed" event
  -- find pc in peerPDataMapD & peerDataMapD
  -- close this pc
  -- send (peer, Nothing) event to pStE & peerPDataE, to clean then in there dynamic

  performEvent_ $ ffor pStE $ \(rp, st) ->
                if stNeedClean st
                then do pm  <- sample $ current peerDataMapD
                        pm1 <- sample $ current peerPDataMapD

                        let pc' = findPeerPc rp pm pm1

                        mapM_ DOM.close pc'

                        liftIO $ pStT (rp, Nothing)
                        liftIO $ peerPDataT (rp, Nothing)
                else return ()


  -- TODO, scan prepared 'pc' periodically, remove then after some time


  performEvent_ $ ffor txMsgE $ \(rp, msg) -> do
     pm <- sample $ current peerDataMapD

     case pm M.!? rp of
       Just (RTCPeerData _ dc) -> do
          -- TODO, use send
          DOM.liftJSM $ RTCDataChannel.sendString dc msg
       Nothing ->
          return ()


  let sendSignalFunc = liftIO . txSdT
  let insertDCFunc rp dc = do
       -- add call back for rxMsgT
       DOM.on dc RTCDataChannel.open $ do
         liftIO $ pStT (rp, Just $ T.pack "open")

       DOM.on dc RTCDataChannel.message $ do
         ev <- DOM.event
         dat <- DOM.getData ev
         liftIO $ rxMsgT (rp, dat)

       DOM.on dc RTCDataChannel.error $ do
         liftIO $ pStT (rp, Just $ T.pack "error")

       DOM.on dc RTCDataChannel.closeEvent $ do
         liftIO $ pStT (rp, Just $ T.pack "closed")

       liftIO $ peerPDataT $ (rp, Just $ Right dc)

  -- TODO, check pc exist, and close exist pc
  -- or ignore is current pc is OK ??
  let insertPCFunc rp x = do
       liftIO $ pStT (rp, Just $ T.pack "prepare")
       liftIO $ peerPDataT $ (rp, Just $ Left x)

  performEvent_ $ ffor (_mqtt_message mqtt) $ \sd -> do
     pm <- sample $ current peerDataMapD
     pm1 <- sample $ current peerPDataMapD

     let rp = signalRemotePeer sd

     let pc' = findPeerPc rp pm pm1
     let dc' = findPeerDc rp pm pm1

     DOM.liftJSM $ procSignalData lp
                                  pc' dc'
                                  sendSignalFunc insertDCFunc insertPCFunc
                                  sd

  performEvent_ $ ffor rpE $ \rp -> DOM.liftJSM $ do
     pc' <-  catch (Just <$> connectPeer lp rp Nothing
                                         sendSignalFunc insertDCFunc)
                   promiseH0
     mapM_ (insertPCFunc rp) pc'

  return $ RTCManager (_mqtt_state mqtt) pStD rxMsgE

mqttWidget :: ( Reflex t
              , MonadSample t m
              , DOM.MonadJSM m
              , MonadHold t m
              , PerformEvent t m
              , TriggerEvent t m
              , MonadIO (Performable m)
              ) =>
              Text -> -- local peer id
              Event t SignalData -> -- send signal data
              m (MqttWidget t)
mqttWidget lp reqE = do
    (msgE, msgT) <- newTriggerEvent

    (stE, stT) <- newTriggerEvent
    stDyn <- holdDyn MqttClosed stE

    client <- DOM.liftJSM (mqttProc stT msgT lp)

    let reqE' = gate ((== MqttReady) <$> current stDyn) reqE

    performEvent $ ffor reqE' $ DOM.liftJSM . sendSignal client

    return $ MqttWidget stDyn msgE

dbgDataChannel :: Text -> DOM.RTCDataChannel -> JSM ()
dbgDataChannel remotePeer dc = do
  --connMap ^. js remotePeer ^. jss "dc" dc
  {-
  DOM.on dc DOM.message $ do
    e <- DOM.event
    DOM.liftJSM $ consoleLog e

  DOM.on dc DOM.open $ do
    e <- DOM.event
    DOM.liftJSM $ consoleLog e
  -}
  return ()

-- unsafe, need catch PromiseReject exception
connectPeer :: Text -> Text ->
               Maybe JSVal ->
               (SignalData -> JSM()) ->
               (Text -> DOM.RTCDataChannel -> JSM()) ->
               JSM DOM.RTCPeerConnection
connectPeer localPeer remotePeer remoteSdp
            sendSignalFunc insertDCFunc = do
  pc <- cfg >>= new (jsg "RTCPeerConnection") >>= (return . DOM.RTCPeerConnection)
  let isOffer = isNothing remoteSdp
  -- DOM.getConfiguration pc >>= consoleLog

  DOM.on pc DOM.iceCandidate $ do
    e <- DOM.event
    c <- DOME.getCandidate e

    DOM.liftJSM $ do
      c' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" c >>= valToStr)
      sendSignalFunc $ SignalData localPeer remotePeer (T.pack "candidate") c'

      done <- valIsNull c
      --unless n $ DOM.liftJSM $ do
      --  x <- DOM.getCandidate c :: JSM String
      --  consoleLog x
      if done then consoleLog "done"
              else consoleLog c

    return ()

  -- NOTE: DOM.dataChannel has wrong type
  DOM.on pc (unsafeEventName (toJSString "datachannel")) $ do
     e <- DOM.event
     dc <- DOME.getChannel e
     DOM.liftJSM $ do
       consoleLog "on data channel"
       insertDCFunc remotePeer dc

  -- save pc & dc, for debug
  {-
  objNew <- obj
  connMap ^. jss remotePeer objNew
  connMap ^. js remotePeer ^. jss "pc" pc
  -}

  -- NOTE: should create dc before offser
  dc' <- if isOffer
        then Just <$> DOM.createDataChannel pc "ch0" Nothing
        else return Nothing

  -- NOTE: should set remote sdp before answer
  forM_ remoteSdp $ \x -> DOM.setRemoteDescription pc $ DOM.RTCSessionDescriptionInit x

  --consoleLog $ connMap ^. js remotePeer

  -- TODO, create ans & off is promise
  sdp <- if isJust remoteSdp
         then DOM.createAnswer pc Nothing
         else DOM.createOffer pc Nothing

  consoleLog sdp

  --- TODO, this is promise, maybe reject
  DOM.setLocalDescription pc sdp
  sdp' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" sdp >>= valToStr)

  -- insert dc, should insert after last function
  -- which throw PromiseRejected exception
  mapM_ (insertDCFunc remotePeer) dc'

  let f op = sendSignalFunc $ SignalData localPeer remotePeer (T.pack op) sdp'

  if isJust remoteSdp then f "answer" else f "offer"


  return pc
