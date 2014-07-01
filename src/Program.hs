module Program
       ( onMasterRealize
       , onCanvasRealize
       , buildNetwork
       ) where

import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Applicative ((<$>))
import Control.Event.Handler
import Data.Bifunctor (bimap)
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

import Rendering


-- The Handlers type contains all the handlers needed to create the reactive network.
-- The fields should correspond with Plugs. For each AddHandler a there has to be an a -> IO ().
data Handlers = Handlers
                { addOnMasterRealize :: AddHandler Canvas
                , addOnCanvasRealize :: AddHandler Canvas
                }

-- The Plugs type contains all the IO actions used to fire events in the reactive network.
-- This is passed out of this module upon calling buildNetwork. The caller must then add event
-- callbacks to the GUI widgets which call these actions as necessary.
data Plugs = Plugs
             { onMasterRealize :: Canvas -> IO ()
             , onCanvasRealize :: Canvas -> IO ()
             }


-- Creates all the handlers and plugs.
makeHandlers :: IO (Handlers, Plugs)
makeHandlers = do
  (r,s) <- bimap Handlers Plugs <$> newAddHandler
  bimap r s <$> newAddHandler


-- This function calls makeHandlers to set up all the handlers and plugs, then calls makeNetwork
-- to set up the actual network using the handlers (which is where all the application logic happens).
-- It then compiles and actuates the network and returns the plugs, which the caller can use to
-- fire events.
buildNetwork :: Table -> IO Plugs
buildNetwork table = do
  (handlers, plugs) <- makeHandlers
  network <- compile (makeNetwork handlers plugs table)
  actuate network
  return plugs


-- Makenetwork builds the reactive application network using the provided handlers.
makeNetwork :: Frameworks t => Handlers -> Plugs -> Table -> Moment t ()
makeNetwork handlers plugs table = do
  eMasterRealize <- fromAddHandler (addOnMasterRealize handlers)
  eCanvasRealize <- fromAddHandler (addOnCanvasRealize handlers)
  let eReady = filterE ((==2) . length) $ accumE [] $ pure (:) <@> eCanvasRealize

  -- When the realize event is called on the canvas, ask the Rendering module to initialize.
  reactimate $ doMaster (onCanvasRealize plugs) table <$> eMasterRealize
  reactimate $ (\_ -> putStrLn "Ready called!") <$> eCanvasRealize
  --reactimate $ doReady <$> eReady
