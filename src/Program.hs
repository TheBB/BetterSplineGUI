module Program
       ( onCanvasReady
       , buildNetwork
       ) where

import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Control.Applicative ((<$>))
import Control.Event.Handler
import Data.Bifunctor (bimap)
import Reactive.Banana
import Reactive.Banana.Frameworks

import Rendering


-- The Handlers type contains all the handlers needed to create the reactive network.
-- The fields should correspond with Plugs. For each AddHandler a there has to be an a -> IO ().
data Handlers = Handlers
                { addOnCanvasReady :: AddHandler Canvas
                }

-- The Plugs type contains all the IO actions used to fire events in the reactive network.
-- This is passed out of this module upon calling buildNetwork. The caller must then add event
-- callbacks to the GUI widgets which call these actions as necessary.
data Plugs = Plugs
             { onCanvasReady :: Canvas -> IO ()
             }


-- Creates all the handlers and plugs.
makeHandlers :: IO (Handlers, Plugs)
makeHandlers = bimap Handlers Plugs <$> newAddHandler


-- This function calls makeHandlers to set up all the handlers and plugs, then calls makeNetwork
-- to set up the actual network using the handlers (which is where all the application logic happens).
-- It then compiles and actuates the network and returns the plugs, which the caller can use to
-- fire events.
buildNetwork :: IO Plugs
buildNetwork = do
  (handlers, plugs) <- makeHandlers
  network <- compile (makeNetwork handlers)
  actuate network
  return plugs


-- Makenetwork builds the reactive application network using the provided handlers.
makeNetwork :: Frameworks t => Handlers -> Moment t ()
makeNetwork handlers = do
  eCanvasReady <- fromAddHandler (addOnCanvasReady handlers)

  -- When the realize event is called on the canvas, ask the Rendering module to initialize.
  reactimate $ canvasReady <$> eCanvasReady
