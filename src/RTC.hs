{-# LANGUAGE ScopedTypeVariables #-}

module RTC where

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.RTCPeerConnection as DOM
import qualified GHCJS.DOM.RTCIceCandidateEvent as DOME
import qualified GHCJS.DOM.RTCDataChannelEvent as DOME
import qualified GHCJS.DOM.RTCIceCandidate as DOM
import qualified GHCJS.DOM.RTCDataChannel as DOM

import qualified GHCJS.DOM.EventM as DOM

import GHCJS.DOM.EventTargetClosures (unsafeEventName)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

import Language.Javascript.JSaddle( askJSM
                                  , JSM(..)
                                  , JSContextRef
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
                                  , strToText
                                  , valToNumber, valIsNull, valToStr
                                  )
import Control.Lens ((^.))
import Control.Monad (when, unless, forM_)
--import Data.Aeson
import Data.List (stripPrefix, findIndex)
import qualified Data.Map as M

import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Maybe (isJust)

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
connMap :: JSM JSVal
connMap = do
    window <- jsg "window"
    window ^. js "_conn_map"

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



mqttClient :: JSM JSVal
mqttClient = jsg "window" ^. js "_mqtt_client"

data SignalData = SignalData { signalLocalPeer :: String --- local peer id
                             , signalRemotePeer :: String -- remote peer id
                             , signalType :: String -- offer, answer, candidate
                             , signalContent :: Text -- sdp or ice_candidate
                             }

sendSignal :: SignalData -> JSM ()
sendSignal d = do
    client <- mqttClient
    let topic = topicPrefix ++ "/" ++
                signalRemotePeer d ++ "/" ++
                signalLocalPeer d ++ "/" ++
                signalType d
    let content = signalContent d

    paho <- jsg "Paho"
    msg <- new (paho ^. js "MQTT" ^. js "Message") content
    msg ^. jss "destinationName" topic

    client ^.js1 "send" msg

    return ()

procSignal :: SignalData -> JSM ()
procSignal d = do
   let c = signalContent d
   let remotePeer = signalRemotePeer d
   let localPeer = signalLocalPeer d

   -- connection data
   cd <- connMap ^. js remotePeer >>= maybeNullOrUndefined

   case (signalType d, cd) of
      ("offer"     , _)       -> procSignalOffer localPeer remotePeer c
      (_           , Nothing) -> return ()
      ("answer"    , Just _)  -> procSignalAnswer remotePeer c
      ("candidate" , Just _)  -> procSignalCandidate remotePeer c
      _ -> return ()

procSignalOffer :: String -> String -> Text -> JSM ()
procSignalOffer localPeer remotePeer sdp = do
    peerConnect localPeer remotePeer (Just sdp)

procSignalAnswer :: String -> Text -> JSM ()
procSignalAnswer peer sdp = do
   -- check
   pc <- connMap ^. js peer ^. js "pc" >>= fromJSValUnchecked

   sdp' <- jsg "JSON" ^. js1 "parse" sdp
   DOM.setRemoteDescription pc $ DOM.RTCSessionDescriptionInit sdp'

   return ()


procSignalCandidate :: String -> Text -> JSM ()
procSignalCandidate peer cand = do
  pc <- (connMap ^. js peer ^. js "pc") >>= fromJSValUnchecked

  cand' <- jsg "JSON" ^. js1 "parse" cand
  DOM.addIceCandidate pc (DOM.RTCIceCandidate cand')

mqttStart :: String -> JSM ()
mqttStart localPeer = do
  consoleLog ("local_peer: ", localPeer)
  console <- jsg "console"
  paho <- jsg "Paho"
  client <- new (paho ^. js "MQTT" ^. js "Client")
                ("test.mosquitto.org", 8080 :: Int, "/", "client" ++ localPeer)
  client ^. jss "onMessageArrived" (fun $ \ _ _ [e] -> do
            payload <- strToText <$> (e ^. js "payloadString" >>= valToStr)
            topic <- strToText <$> (e ^. js "topic" >>= valToStr)
            case parseTopic (topicPrefix ++ "/") (T.unpack topic) of
                 Just (lid, rid, op) -> do
                      consoleLog (lid, rid, op, payload)
                      procSignal (SignalData lid rid op payload)
                 Nothing -> return ()
            )

  client ^. jss "onConnected" (fun $ \_ _ [rc, uri] -> do
      console ^. js1 "log" ("connected:", uri)
      client ^. js1 "subscribe" (topicPrefix ++ "/" ++ localPeer ++ "/#")
      return ())

  client ^. jss "onConnectionLost" (fun $ \_ _ [ec, em] -> do
         console ^. js1 "log" ("lost connection", ec, em)
         liftIO $ threadDelay $ 2000 * 1000
         console ^. js1 "log" "reconnect"
         client ^. js0 "connect"

         return ())

  client ^. js0 "connect" -- cb


  window <- jsg "window"
  window ^.jss "_mqtt_client" client

  return ()

initDataChannel :: String -> DOM.RTCDataChannel -> JSM ()
initDataChannel remotePeer dc = do
  connMap ^. js remotePeer ^. jss "dc" dc

  DOM.on dc DOM.message $ do
    e <- DOM.event
    DOM.liftJSM $ consoleLog e

  DOM.on dc DOM.open $ do
    e <- DOM.event
    DOM.liftJSM $ consoleLog e

  return ()

peerConnect :: String -> String -> Maybe Text -> JSM ()
peerConnect localPeer remotePeer remoteSdp = do
  pc <- cfg >>= new (jsg "RTCPeerConnection") >>= (return . DOM.RTCPeerConnection)
  let isOffer = remoteSdp == Nothing
  -- DOM.getConfiguration pc >>= consoleLog

  DOM.on pc DOM.iceCandidate $ do
    e <- DOM.event
    c <- DOME.getCandidate e

    DOM.liftJSM $ do
      c' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" c >>= valToStr)
      sendSignal $ SignalData localPeer remotePeer "candidate" c'

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
       initDataChannel remotePeer dc

  -- TODO, check connmap ^. js remotePeer, release when necessary
  isClean <- connMap ^. js remotePeer >>= valIsNull
  unless isClean $ return () -- TODO, clean already run connection

  -- save pc & dc
  dcNew <- obj
  connMap ^. jss remotePeer dcNew
  connMap ^. js remotePeer ^. jss "pc" pc

  -- NOTE: should create dc before offser
  when isOffer $ do
    dc <- DOM.createDataChannel pc "ch0" Nothing
    initDataChannel remotePeer dc

  -- NOTE: should set remote sdp before answer
  forM_ remoteSdp $ \x -> do
    -- TODO, do check
    x1 <- jsg "JSON" ^. js1 "parse" x
    DOM.setRemoteDescription pc $ DOM.RTCSessionDescriptionInit x1

  consoleLog $ connMap ^. js remotePeer

  sdp <- if isJust remoteSdp
         then DOM.createAnswer pc Nothing
         else DOM.createOffer pc Nothing

  consoleLog sdp

  DOM.setLocalDescription pc sdp
  sdp' <- strToText <$> (jsg "JSON"  ^. js1 "stringify" sdp >>= valToStr)

  if isJust remoteSdp
     then sendSignal $ SignalData localPeer remotePeer "answer" sdp'
     else sendSignal $ SignalData localPeer remotePeer "offer" sdp'

  return ()
