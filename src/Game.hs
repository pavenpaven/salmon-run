{-# LANGUAGE BlockArguments #-}

module Game where

import SDL
import SDL.Font hiding (Color)
import SDL.Mixer (Channel, defaultAudio, ChunkSize, withAudio)
import Foreign.C.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State as St
import Control.Monad.Trans.Reader
import Control.Arrow

import Data.Word
import Data.Int
import qualified Data.Text as T
import Data.Vector
import Data.Monoid

import System.IO.Unsafe

import Debug.Trace

windowConfig :: WindowConfig
windowConfig = defaultWindow {
                              windowPosition = Centered
                             ,windowGraphicsContext = OpenGLContext defaultOpenGL}


type Color = V4 Word8


white  = V4 255 255 255 0 :: Color
red    = V4 255 0   0   0 :: Color
blue   = V4 0   0   255 0 :: Color
green  = V4 0   255 0   0 :: Color
black  = V4 0   0   0   0 :: Color
orange = V4 255 165 0   0 :: Color
purple = V4 128 0   128 0 :: Color

average :: Color -> Color -> Color
average v w = halfVector v + halfVector w
  where halfVector :: Color -> Color
        halfVector (V4 a b c d) = V4 (a `div` 2) (b `div` 2) (c `div` 2) d

dark :: Color -> Color
dark = average black

light :: Color -> Color
light = average white

type RenderingContext = ReaderT Renderer IO

newtype GraphicalProcess = MkGraphicalProcess (Kleisli IO Renderer Renderer)

type Rect = (V2 Int, V2 Int)

type Pnt = V2 Int

instance Semigroup GraphicalProcess where
  (MkGraphicalProcess a) <> (MkGraphicalProcess b) = MkGraphicalProcess (a >>> b)
  
instance Monoid GraphicalProcess where
  mempty = MkGraphicalProcess $ Kleisli (\r -> return r)

convertToV2CInt :: V2 Int -> V2 CInt
convertToV2CInt (V2 x y) = V2 (fromIntegral x) (fromIntegral y)


stdRendering :: (Renderer -> IO a) -> Color -> GraphicalProcess
stdRendering a color = MkGraphicalProcess (Kleisli (\r -> (rendererDrawColor r $= color
                                                       >> a r  
                                                       >> return r)))

textSize :: Font -> String -> (Int, Int)
textSize f s = unsafePerformIO $ SDL.Font.size f (T.pack s) -- Sinice this function should like be pure evan though it uses IO I think its worf making use of unsafePerformIO

text :: Font -> Color -> V2 Int -> String -> GraphicalProcess
text font color pos s = MkGraphicalProcess $ Kleisli
  (\r -> do{ sur <- solid font color (T.pack (if Prelude.null s then " " else s))
          ; tex <- createTextureFromSurface r sur
          ; dim <- surfaceDimensions sur
          ; SDL.copy r tex Nothing (Just $ Rectangle (P $ convertToV2CInt pos) dim)
          ; freeSurface sur
 
          ; destroyTexture tex
          ; return r})
     

  
rect :: Color -> Rect -> GraphicalProcess
rect color (pos, size) = stdRendering (\r -> fillRect r (Just (Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)))) color

outline :: Color -> Rect -> GraphicalProcess
outline color (pos, size) = stdRendering (\r -> drawRect r (Just (Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)))) color

line :: Color -> Pnt -> Pnt -> GraphicalProcess
line color p1 p2 = stdRendering (\r -> drawLine r (P $ convertToV2CInt p1) (P $ convertToV2CInt p2)) color

textureDimensions :: Texture -> V2 Int
textureDimensions tex = unsafePerformIO (fmap (\x -> V2 (fromIntegral $ textureWidth x) (fromIntegral $ textureHeight x)) $ queryTexture tex)

point :: Color -> Pnt -> GraphicalProcess
point color p = stdRendering (\r -> drawPoint r (P $ convertToV2CInt p)) color

display :: Texture -> Rect -> GraphicalProcess
display t (pos, size) = MkGraphicalProcess $ Kleisli (\r -> SDL.copy r t Nothing (Just $ Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)) >> return r)

displayEx :: Texture -> Rect -> Double -> V2 Bool -> GraphicalProcess
displayEx t (pos, size) rot flip = MkGraphicalProcess $ Kleisli (\r -> SDL.copyEx r t Nothing (Just $ Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)) (CDouble rot) Nothing flip >> return r)

displayRotated :: Texture -> Rect -> Double -> GraphicalProcess
displayRotated t r d = displayEx t r d (V2 False False)

defaultRendering :: (Renderer -> IO a) -> RenderingContext a
defaultRendering f = ask >>= liftIO . f

load :: FilePath -> RenderingContext Texture
load p = defaultRendering (\r -> loadBMP p >>= createTextureFromSurface r)

data GameConfig = GameConfig { fps         :: Int
                             , windowConf  :: WindowConfig
                             , renderConf  :: RendererConfig
                             , defaultBkg  :: V4 Word8
                             , windowOn    :: Bool
                             , allowResize :: Bool}

defaultConfig :: GameConfig
defaultConfig = GameConfig { fps = 60
                           , windowConf  = windowConfig
                           , renderConf  = defaultRenderer
                           , defaultBkg  = V4 0 0 0 255
                           , windowOn    = True
                           , allowResize = False}


loopTimedExit :: (MonadIO m) => Int -> (Float -> m Bool) -> m () -- needs to be initialized the sdl library since it uses tick
loopTimedExit fps up =
  do t <- liftIO ticks
     exits <- up 0 -- FIXME

     t' <- liftIO ticks

     let delta = t' - t
     let mspf = fromIntegral (1000 `div` fps)

     liftIO $ delay (mspf - min delta mspf)
  
     unless exits $ loopTimedExit fps up


loopTimed :: (MonadIO m) => Int -> (Float -> m a) -> m ()
loopTimed fps up = loopTimedExit fps (\f -> up f >> return False) -- this function cost me like an hour of debugging or more


runGame :: GameConfig -> (RenderingContext gamestate) -> (gamestate -> Int -> GraphicalProcess) -> (gamestate -> Float -> gamestate) -> (Event -> gamestate -> gamestate) -> IO ()
runGame config game draw update eventHandle = runGameIO config game draw (\x -> \y -> return $ update x y) eventHandle

chunkSize :: ChunkSize  -- dunno what a good value is
chunkSize = 16

runGameIO :: GameConfig -> (RenderingContext gamestate) -> (gamestate -> Int -> GraphicalProcess) -> (gamestate -> Float -> IO gamestate) -> (Event -> gamestate -> gamestate) -> IO ()
runGameIO config game draw update eventHandle =
  initializeAll >>
  withAudio defaultAudio chunkSize (
  do SDL.Font.initialize
     win <- createWindow (T.pack "test") $ windowConf config
     render <- createRenderer win (-1) $ renderConf config

     g <- runReaderT game render
     gameLoop config render g draw update eventHandle

     destroyRenderer render -- dunno if i have to do it but its sdl so who knows
     destroyWindow win)

{-
I spent a while thinking about the text and like oh no I have to createTextureFromSurface and 
render the text every frame so I would either have to add a rendering field as an argument
and scedual rendering updates or make some kind of cache for the but then I just realized that
who cares its probobly fine.
-}
type Seconds = Float

gameStep :: GameConfig -> Renderer -> (gamestate -> Int -> GraphicalProcess) -> (gamestate -> Float -> IO gamestate) -> (Event -> gamestate -> gamestate) -> gamestate -> Int -> Seconds -> IO (gamestate, Bool, Int) 
gameStep gameConf renderer draw update eventHandle game framecount delta =
  do events <- pollEvents
     
     game'  <- update game delta
     let game'' = foldl' (flip eventHandle) game' (Data.Vector.fromList events)

     let exits = Prelude.any isExitEvent events 

     rendererDrawColor renderer $= defaultBkg gameConf
     clear renderer

     (runProcess $ draw game'' framecount) renderer
     present renderer
     return (game'', exits, framecount + 1)
  where
    isExitEvent :: Event -> Bool
    isExitEvent e = case eventPayload e of
                     WindowClosedEvent a -> True
                     _                   -> False

--    isResize    :: Event -> Bool

gameLoop :: GameConfig -> Renderer -> gamestate -> (gamestate -> Int -> GraphicalProcess) -> (gamestate -> Float -> IO gamestate) -> (Event -> gamestate -> gamestate) -> IO ()
gameLoop gameConf renderer game draw update eventHandle =
     ticks >>= \initialTicks ->
     runStateT (loopTimedExit (fps gameConf) $ \f -> do (g, framecount, t) <- St.get
                                                        t' <- ticks
                                                        let delta = fromIntegral (t' - t) / 1000 -- conversion to seconds
                                                        (g', shouldExit, framecount') <- liftIO $ gameStep gameConf renderer draw update eventHandle g framecount delta
                                                        
                                                        St.put (g', framecount', t')
                                                        return shouldExit) (game, 0, initialTicks) >> return ()

runProcess :: GraphicalProcess -> Renderer -> IO ()
runProcess (MkGraphicalProcess g) r = runKleisli g r >> return ()
