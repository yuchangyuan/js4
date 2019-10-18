{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language RecursiveDo  #-}

module Main where

import Control.Concurrent (threadDelay)

import Crypto.ECC.Ed25519.Sign
import qualified Data.ByteString.Char8 as B

import Language.Javascript.JSaddle( askJSM
                                  , JSM(..)
                                  , JSContextRef
                                  , syncPoint
                                  , runJSM
                                  , runJSaddle
                                  , val, fun
                                  , js, jss, jsf
                                  , js0, js1, js2, jsg
                                  , valToNumber, valToText
                                  )
import Control.Lens ((^.))

import Control.Concurrent.MVar
import Data.Time(getCurrentTime)
import qualified System.IO as IO

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.NonElementParentNode as DOM
import qualified GHCJS.DOM.EventTarget as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

import Control.Monad (forever, when, join)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

import qualified Data.Text as T
import Data.Text (Text(..))

import System.IO (hFlush, stdout)
import qualified Data.Map as M
import Data.Map (Map(..))

       {-

import Reflex.Class
import Reflex.Dynamic
import Reflex.Dom.Builder.Class
-}
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Location (getLocationPath, getLocationAfterHost)

import Control.Monad.Fix (MonadFix)

import Control.Lens
import Control.Monad.Catch (MonadCatch, catch) -- JSM is MonadCatch

import RTC

{-
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson (encode)
-}

{-
1. update mouse position
2. click count
3. reload after some click
4. should location
-}

a1 :: (Reflex t, MonadHold t m, MonadFix m) => m ()
a1 = return ()

  -- posSpan <- DOM.getElementByIdUnsafe doc posId
  -- DOM.setAttribute posSpan "style"
  --                  $ concat [ "position:fixed;"
  --                           , "bottom:5px;"
  --                           , "left: 5px;"
  --                           , "width: 100%;"
  --                           , "background-color:rgba(255,255,255,0.8);"
  --                           ]


reloadPage :: DOM.MonadJSM m => m ()
reloadPage = do
  doc <- DOM.currentDocumentUnchecked
  loc <- DOM.getLocationUnsafe doc
  DOM.reload loc
  liftIO syncPoint


mouseMoveEv :: (TriggerEvent t m, DOM.MonadJSM m) => m (Event t (Int, Int))
mouseMoveEv = do
  (ev, trig) <- newTriggerEvent

  doc <- DOM.currentDocumentUnchecked
  liftIO $ DOM.on doc DOM.mouseMove $ do
         x <- DOM.mouseClientX
         y <- DOM.mouseClientY
         liftIO $ trig (x, y)

  return ev

clickEv :: (TriggerEvent t m, DOM.MonadJSM m) => m (Event t ())
clickEv = do
   doc <- DOM.currentDocumentUnchecked

   (ev, trig) <- newTriggerEvent
   liftIO $ DOM.on doc DOM.click $ liftIO $ trig ()
   return ev


boolEnable :: Bool -> Map Text Text
boolEnable x = if x then M.empty else "disabled" =: "disabled"

rtcWidget ::
     ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , MonadFix m
     , Reflex t
     , PostBuild t m
     , DOM.MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     , MonadSample t (Performable m)
     ) => m ()
rtcWidget = mdo
  elAttr "p" ("style" =: "color:blue;") $ text "set local peer ID"

  let lpTxtAttr = (boolEnable . (== MqttClosed)) <$> join (_rtc_mqtt_state <$> rtcManager_d)
  lpTxt <- textInput $ TextInputConfig "" "" never lpTxtAttr

  let lpBtnAttr = zipDynWith (\a b -> boolEnable $ (a /= T.empty) && (b == MqttClosed))
                                    (_textInput_value lpTxt)
                                    (join $ _rtc_mqtt_state <$> rtcManager_d)

  text " "
  (lpBtn, _) <- elDynAttr' "button" lpBtnAttr (text "set local peer")

  let rtcManagerInit_e = ffor (tag (current $ _textInput_value lpTxt)
                                   (domEvent Click lpBtn)) $
                         \lp -> rtcManagerNew lp rpE txMsgE

  rtcManager_d <- widgetHold (return rtcManagerDummy) rtcManagerInit_e

  el "p" $ do text "signal server state: "
              dynText $ fmap (T.pack . show) $ join $ _rtc_mqtt_state <$> rtcManager_d

  elAttr "p" ("style" =: "color:blue;") $ text "connect to another peer"

  let rpTxtAttr = (boolEnable . (== MqttReady)) <$> join (_rtc_mqtt_state <$> rtcManager_d)
  rpTxt <- textInput $ TextInputConfig "" "" never rpTxtAttr

  let rpBtnAttr = zipDynWith (\(a,a1) b ->  boolEnable $ (a /= T.empty) &&
                                                         (a /= a1) &&
                                                         (b == MqttReady))
                                (zipDyn (_textInput_value rpTxt)
                                        (_textInput_value lpTxt))
                                (join $ _rtc_mqtt_state <$> rtcManager_d)

  text " "
  (rpBtn, _) <- elDynAttr' "button" rpBtnAttr $ text "connect peer"

  elAttr "p" ("style" =: "color:blue;") $ text "send message to any connected peer"

  let rpE = tag (current $ _textInput_value rpTxt) (domEvent Click rpBtn)

  let state_dyn = join $ _rtc_peer_state <$> rtcManager_d

  -- Map Text Text
  msgTA <- el "div" $ do el "p" $ text "message content: "
                         textArea $ TextAreaConfig "" never
                                                   (constDyn $ ("cols" =: "60") <>
                                                               ("rows" =: "10"))

  txMsgE <- genList state_dyn (_textArea_value msgTA)

  rxMsg_e <- switchHold never $ updated $ _rtc_rx_msg <$> rtcManager_d
  performEvent_ $ ffor rxMsg_e $ \m -> DOM.liftJSM $ consoleLog m

  elAttr "p" ("style" =: "color:blue;") $ text "received message (latest first)"

  -- Dynamic t [(Text, Text)]
  rxMsg_dyn <- foldDyn (:) [] rxMsg_e

  let rxMsg_w1 =  ffor rxMsg_dyn $ \l -> mapM_ (\(a, b) -> el "li" $ do
                    elAttr "span" ("style" =: "color:red") $ text a
                    text " "
                    m <- DOM.liftJSM $ valToText b
                    text m) l
  -- dyn :: Dynamic t (m a)
  el "ul" $ dyn_ rxMsg_w1

  return ()

genEntry :: ( DomBuilder t m
            , Reflex t
            ) =>
            Text -> Text ->
            m (Event t Text) -- peerid
genEntry peer state = do
   el "tr" $ do el "td" $ text peer
                el "td" $ text state
                e <- el "td" $ button $ T.concat [T.pack "send to ", peer]
                return $ fmap (const peer) $ traceEvent "btn s " e

genList :: ( DomBuilder t m
           , Reflex t
           , PostBuild t m
           , MonadHold t m
           ) =>
           Dynamic t (Map Text Text) ->
           Dynamic t Text ->
           m (Event t (Text, Text)) -- m (Event t (peerid, text))
genList m_dyn txt_dyn = do
              let f (k, v) = do e0 <- genEntry k v
                                let e1 = attach (current txt_dyn) e0
                                let e2 = fmap (\(x,y) -> (y,x)) e1 -- Event t (Text, Text)
                                return e2

              e <- elAttr "table" ("border" =: "1" <>
                                   "style" =: "border: 1px;") $ do
                           el "tr" $ do el "th" $ text "remote peer"
                                        el "th" $ text "state"
                                        el "th" $ text "send message"
                           -- Dynamic t (m (Event t (Text, Text)))
                           let a = (fmap leftmost . mapM f) <$> (M.toList <$> m_dyn)
                           b <- dyn a -- Event t (Event t (Text, Text))
                           c <- switchHold never b -- Event t (Text, Text)
                           return c

              return $ traceEvent "ev list" e

app
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , MonadFix m
     , PostBuild t m
     , DOM.MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     , MonadSample t (Performable m)
     ) => m ()
app = do
    rtcWidget
    -- mouse location
    el "hr" blank

    mEv <- mouseMoveEv
    dynMousePos <- holdDyn "mouse pos: " $ ffor mEv $ ("mouse pos: " <>) . T.pack . show
    dynText dynMousePos

    text " "
    -- elapse
    now <- liftIO $ getCurrentTime
    sec1s <- tickLossy (fromIntegral 1) now

    secCnt <- foldDyn (const (+ 1)) 0 sec1s

    dynText $ ffor secCnt $ ("elapse: " <>) . T.pack . show

main :: IO ()
main = mainWidget $ do
   DOM.liftJSM rtcInit
   app
