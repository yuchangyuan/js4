{-# Language TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

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
                                  , valToNumber
                                  )
import Control.Lens ((^.))

import Control.Concurrent.MVar
import Data.Time(getCurrentTime)
import qualified System.IO as IO
import Control.Monad ( when )

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

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import qualified Data.Text as T

import System.IO (hFlush, stdout)
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

app
  :: ( DomBuilder t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , MonadFix m
     , PostBuild t m
     , DOM.MonadJSM m
     , PerformEvent t m
     , MonadIO (Performable m)
     ) => m ()
app = do
    loc1 <- getLocationPath
    loc2 <- getLocationAfterHost
    el "p" $ text loc1
    el "p" $ text loc2

    -- mouse location
    mEv <- mouseMoveEv
    dynMousePos <- holdDyn "mouse pos: " $ ffor mEv $ ("mouse pos: " <>) . T.pack . show
    el "p" $ dynText dynMousePos

    -- click cnt
    cEv <- clickEv
    cnt <- foldDyn (const (+ 1)) 0 cEv

    el "p" $ dynText $ ffor cnt $ ("cnt : " <>) . T.pack . show

    -- reload
    --performEvent_ $ ffor (updated cnt) $ \c -> liftIO $ print c >> hFlush stdout

    btn <- button "reload"
    performEvent_ $ ffor btn $ \c -> reloadPage

    -- elapse
    now <- liftIO $ getCurrentTime
    sec1s <- tickLossy (fromIntegral 1) now

    secCnt <- foldDyn (const (+ 1)) 0 sec1s

    el "p" $ dynText $ ffor secCnt $ ("elapse: " <>) . T.pack . show

main :: IO ()
main = mainWidget app
