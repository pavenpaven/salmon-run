{-# LANGUAGE DeriveGeneric #-}

module Main where

import SDL hiding (trace)
import SDL.Font (Font)
import qualified SDL.Font as F
import SDL.Mixer (Channel, defaultAudio, ChunkSize
                 , withAudio, Chunk, Music
                 , chunkDecoders, setVolume, play
                 , playOn, halt)
import qualified SDL.Mixer as M

import Game

import Menu hiding (Pos)

import Data.Aeson
import Data.Monoid
import Data.Maybe
import Data.Either
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Control.Monad
import Control.Monad.IO.Class

import System.IO
import System.Directory
import System.Environment
import System.Random

import GHC.Generics

import Debug.Trace

data TileConfig = MkTileConfig {filepath :: FilePath}
  deriving (Eq, Show, Generic)

instance FromJSON TileConfig

type Pos = (Int,Int)

type TileID = Int

type TileMap = [[TileID]]

data Config =
  MkConfig {tile_dimensions :: (Int, Int)
           ,tile_size       :: Int
           ,scene_dict      :: FilePath
           ,meta_data_dict  :: FilePath
           ,art_dict        :: FilePath
           ,fontPath        :: FilePath
           ,fontSize        :: Int
           }
  deriving (Eq, Show, Generic)

instance FromJSON Config

type Vel = Pos

data Entity = MkEntity Pos Vel
  deriving (Eq, Show)

pos :: Entity -> Pos
pos (MkEntity p _) = p

vel :: Entity -> Vel
vel (MkEntity _ v) = v

safeIndex :: [a] -> Int -> Maybe a -- requires finite list
safeIndex a i = if (length a > i) && (i >= 0) then Just (a !! i) else Nothing

type Animation = [Texture]

data State = OverWorld
           | LevelTrans
           | TitleScreen (Menu ())
           | Ending

instance Eq State where
  OverWorld == OverWorld         = True
  LevelTrans == LevelTrans       = True
  TitleScreen _ == TitleScreen _ = True
  Ending == Ending               = True
  _ == _                         = False

data GameState =
  MkGameState {textures        :: [Animation]
              ,sounds          :: [Chunk]
              ,shouldPlay      :: [Bool]
              ,activeMusic     :: (Int, Int)
              ,textFont        :: F.Font
              ,tileMap         :: TileMap
              ,state           :: State
              ,clone           :: Either (Pos, Bool) Entity
              ,boxes           :: [Entity]
              ,moves           :: [(Int,Int)]
              ,player          :: Entity
              ,history         :: [(Either (Pos, Bool) Entity, [Entity], [(Int,Int)], Entity)]
              ,levelTransDelay :: Int
              ,framesSinceTick :: Int
              ,currentLevel    :: Int
              }

tryUndo :: GameState -> GameState
tryUndo g = if null $ history g then g
            else let h'            = tail (history g)
                     (c, b, m, p)  = head (history g)
                 in g {history = h', clone = c, boxes = b, moves = m, player = p}

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left a)  = Nothing
eitherToMaybe (Right a) = Just a

entities :: GameState -> [Entity]
entities g = [player g] ++ (fromMaybe [] $ fmap singleton $ eitherToMaybe $ clone g) ++ boxes g  
  
screenWidth = 13 * tileWidth

screenHeight = 12 * tileWidth

tileWidth :: Int
tileWidth = 64

fontFilePath  = "Art/m3x6.ttf"
fontPointSize = 70 :: Int


replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs


safeTile :: Pos -> TileMap -> Maybe TileID
safeTile (x,y) tilem = join (fmap (`safeIndex` x) (tilem `safeIndex` y))

getTile :: Pos -> TileMap -> TileID
getTile p = fromJust . safeTile p

draw :: GameState -> Int -> GraphicalProcess
draw g frameCount = case state g of
                      OverWorld      -> tileGrid <> drawBoxes <> drawClone <> drawPlayer <> drawMoves
                      LevelTrans     -> levelTransDraw <> text font black (V2 80 30) "Press any key to contiue to next level"
                      TitleScreen m  -> tileGridTitle <> drawFishBoxes <> drawMenu font (concat textrs) m <> title
                      Ending         -> drawEnd <> text font black (V2 80 30) "Press any key to return to title screen"
  where (MkGameState textrs _ _ _ font tilem s c b m (MkEntity (x, y) _) _ ltd _ _) = g

        drawTile :: TileID -> Pos -> GraphicalProcess
        drawTile tileid (x, y) = display ((textrs !! tileid) !! ((frameCount `div` animDelay) `mod` length (textrs !! tileid)))
                                     (V2 (tileWidth * x) (tileWidth * (y + 2)), V2 tileWidth tileWidth)

        drawMoves  = mconcat
                     $ map (\(u, i) -> displayRotated (head (last textrs)) (V2 ((i+1)*tile) 32 , V2 tile tile) (moveAngle u))
                     $ zip m [0..]

        moveAngle :: (Int, Int) -> Double
        moveAngle (0,  1) = 180
        moveAngle (1,  0) = 90
        moveAngle (-1, 0) = 270
        moveAngle (0, -1) = 0
        moveAngle _       = 45
        
        drawPlayer = drawTile playerText (x,y)

        drawClone  = case c of
                       Left (p, _)          -> drawTile 2 p
                       Right (MkEntity p _) -> drawTile 14 p

        drawBoxes  = mconcat $ map (\(MkEntity p _) -> drawTile 3 p) $ boxes g

        tile = tileWidth
        
        tileGrid :: GraphicalProcess
        tileGrid = mconcat
                 $ map (\(y, (x, tile)) -> drawTile tile (x, y))
                 $ concat
                 $ map (\(y, tiles) -> zip (repeat y) tiles)
                 $ zip [0..] (map (zip [0..]) tilem)

        tileGridTitle :: GraphicalProcess
        tileGridTitle = mconcat
                        $ map (\(y, (x, tile)) -> drawTile tile (x, y - 2))
                        $ concat
                        $ map (\(y, tiles) -> zip (repeat y) tiles)
                        $ zip [0..] (map (zip [0..]) tilem)
                 

        playerText = 4

        levelTransDraw = display levelTransText (V2 0 0, V2 screenWidth screenHeight)

        levelTransText = head $ last $ init textrs

        drawFishBoxes = mconcat $ map (\x -> let (a,b) = pos x in drawTile playerText (a,b-2)) $ b
        
        title = display (head $ last $ init $ init textrs) (V2 50 20, V2 (144*5) (48*5))

        drawEnd = display (head $ last $ init $ init $ init textrs) (V2 0 0, V2 screenWidth screenHeight)

loadScene :: FilePath -> IO TileMap
loadScene path = fmap fromJust $ decodeFileStrict ("Level/Scenes/" ++ path)

tileMapSearch :: (TileID -> Bool) -> TileMap -> [Pos]
tileMapSearch f tilem = concat $ zipWith (\i -> map (\x' -> (x', i))) [0..] (map (findIndices f) tilem)


loadLevel :: FilePath -> GameState -> IO GameState
loadLevel p g = fmap (\t -> g {boxes = boxGen t, player = playerGen t, clone = cloneGen t, tileMap = tileGen t, history = [], moves = []}) $ loadScene p
  where tileGen :: TileMap -> TileMap 
        tileGen t = (map . map) (\x -> if x `elem` [2,3,4] then 1 else x) t

        playerGen :: TileMap -> Entity
        playerGen t = let xs = tileMapSearch (== 4) t
                          in if null xs then MkEntity (3,3) (0,0)
                             else MkEntity (head xs) (0,0)

        cloneGen  :: TileMap -> Either (Pos, Bool) Entity
        cloneGen t = let xs = tileMapSearch (== 2) t
                          in if null xs then Left ((-100,3), False)
                             else Left (head xs, False)

        boxGen    :: TileMap -> [Entity]
        boxGen t  = map (\x -> MkEntity x (0,0)) $ tileMapSearch (== 3) t
          

onGoingTick :: GameState -> Bool
onGoingTick = any (((0,0) /=) . vel) . entities


tickFrameDelay :: Int
tickFrameDelay = 5

update :: GameState -> Float -> IO GameState
update g _ = handleMusic =<< (case state g of
                                OverWorld     -> soundProcess =<< (fmap (setMusic 11) overWorld)
                                TitleScreen _ -> fmap (setMusic 12) (if levelTransDelay g == 1
                                                                    then loadLevel "level0" (g {state = OverWorld
                                                                                               , levelTransDelay = 0
                                                                                               , shouldPlay = map (const False) $ shouldPlay g})
                                                                    else titleLax)
                                LevelTrans    -> soundProcess (setMusic (-1) g)
                                Ending        -> return $ setMusic 13 g)
  where overWorld = levelProcess $ timedTick g

        handleMusic :: GameState -> IO GameState
        handleMusic g = let (a,b) = activeMusic g
                        in if a /= b then if b == (-1)
                                          then halt   (fromIntegral 5) >> return (g {activeMusic = (b,b)})
                                          else playOn (fromIntegral 5) (fromIntegral 100000) (sounds g !! b) >> return (g {activeMusic = (b,b)})

                           else return g
        
        setMusic :: Int -> GameState -> GameState
        setMusic i g = let (x,y) = activeMusic g in g {activeMusic = (y, i)}

        soundProcess g' = (mapM (\(_, s) -> play s) $ filter (\(b, _) -> b) $ zip (shouldPlay g') $ sounds g')  >> return (g' {shouldPlay = take (length $ sounds g') $ repeat False})

        levelProcess :: GameState -> IO GameState
        levelProcess g' = if levelTransDelay g' > 0
                          then if levelTransDelay g' == 1
                               then ((if currentLevel g' == 9 then (\s -> return (s {state = Ending, currentLevel = 0})) else loadLevel ("level" ++ show (currentLevel g')))
                                      $ (if state g == OverWorld then queueSound 6 else id)
                                       g {state = if state g == OverWorld then LevelTrans else OverWorld
                                        , levelTransDelay = 0})
                               else return (g' {levelTransDelay = levelTransDelay g' - 1})
                          else return g'
                               
        timedTick g' = if (onGoingTick g' && (framesSinceTick g' >= tickFrameDelay)
                       && levelTransDelay g' == 0)
                    then tick g' {framesSinceTick = 0}
                    else g' {framesSinceTick = framesSinceTick g' + 1}

        titleLax = do r <- randomIO :: IO Int
                      v <- randomIO :: IO Int
                      let g' = if (fromIntegral (r `mod` 1000)) < ((10 - (fromIntegral $ length $ boxes g))**3)
                               then g {boxes = boxes g ++ singleton (spawnRandomLax r)} else g
                      return (timedTick g')

        spawnRandomLax r = let xs = [(0,i) | i <- [0..11]] ++ [(12,i) | i <- [0..11]] ++ [(i,0) | i <- [1..11]] ++ [(i,11) | i <- [1..11]]
                               (x,y) = xs !! (r `mod` length xs)
                             in if x == 0 then MkEntity (x,y) (1,0) else if x == 12 then MkEntity (x,y) (-1,0)
                           else if y == 0 then MkEntity (x,y) (0,1) else MkEntity (x,y) (0, -1)
        

walk :: (Int, Int) -> GameState -> GameState
walk (u, v) g = if onGoingTick g
                then g
                else let MkEntity p _ = player g
                     in g {player = MkEntity p (u,v)
                         , clone = fmap (\(MkEntity a _) -> MkEntity a (head $ moves g)) $ clone g
                         , moves = keep 3 $ (moves g ++ if record then [(u,v)] else [])
                         , history = (clone g, boxes g, moves g, player g) : history g}
  where keep n = reverse . take n . reverse

        record = case clone g of
                   Right a    -> True
                   Left (_,b) -> b
                      
queueSound :: Int -> GameState -> GameState
queueSound i g = g {shouldPlay = replace i True $ shouldPlay g}


tick :: GameState -> GameState
tick g = hitSounds g $ levelTrans $ subtick g
  where subtick :: GameState -> GameState
        subtick = moveEntities . resolveCollisions . activateEgg . filterAliveBoxes

        filterAliveBoxes g = g {boxes = filter (\(MkEntity (a,b) _) -> (a>=0)&&(a<13)&&(b>=0)&&(b<12)) $ boxes g}

        levelTrans :: GameState -> GameState
        levelTrans g = if levelComplete g
                       then queueSound 10 (g {currentLevel = currentLevel g + 1, levelTransDelay = 60})
                       else g

        levelComplete :: GameState -> Bool
        levelComplete g = (all (`elem` map pos (player g : fromMaybe [] (fmap singleton $ eitherToMaybe $ clone g)))
                          $ tileMapSearch (== 5) (tileMap g)) && (state g == OverWorld)

        activateEgg  :: GameState -> GameState
        activateEgg g = case clone g of
                          Right a     -> g
                          Left (p, _) -> if pos (player g) == p
                                         then g {clone = Left (p, True)}
                                         else if length (moves g) >= 3
                                              then queueSound 9 (g {clone = Right (MkEntity p (0,0))})
                                              else g

        moveEntities :: GameState -> GameState
        moveEntities = updateEntities moveEntity

        updateEntities :: (Entity -> Entity) -> GameState -> GameState
        updateEntities f g = g {boxes = map f $ boxes g, clone = fmap f $ clone g, player = f $ player g }

        resolveCollisions :: GameState -> GameState
        resolveCollisions g = run g (entities g)
          where run g es = let g' = collisionPass g
                            in if entities g' == es
                               then g'
                               else run g' (entities g')

        collisionPass :: GameState -> GameState
        collisionPass = neighbouringReflection . tileCollision . movementCanselation


        movementCanselation :: GameState -> GameState
        movementCanselation g = updateEntities (\(MkEntity (x,y) (w,h)) -> if stoneChain g (x,y) (w, h)
                                                                           then MkEntity (x,y) (0,0)
                                                                           else MkEntity (x,y) (w,h)) g

        stoneChain :: GameState -> Pos -> Vel -> Bool
        stoneChain g (x,y) (w,h) = if safeTile (x+w, y+h) (tileMap g) `elem` map Just [0,6,7,8,9,10,11,12,13]
                                   then True
                                   else if ((x+w, y+h) `elem` map pos (entities g)) && ((w,h) /= (0,0))
                                        then stoneChain g (x+w,y+h) (w,h)
                                        else False
  
        tileCollision :: GameState -> GameState
        tileCollision g = updateEntities (\x -> let collisions = filter ((== pos (moveEntity x)) . pos . moveEntity)
                                                                      $ filter (/=x) $ entities g
                                                  in if null collisions then x
                                                     else if any ((== neg (vel x)) . vel) collisions
                                                          then let (MkEntity a v) = x in MkEntity a (neg v)
                                                          else if length collisions == 2 then x
                                                               else let (MkEntity a _) = x
                                                                    in MkEntity a (vel $ head collisions)) g

        neg :: Vel -> Vel
        neg (x,y) = (-x,-y)                                       

        moveEntity :: Entity -> Entity
        moveEntity (MkEntity (x,y) (w,h)) = MkEntity (x + w, y + h) (w,h)

        neighbouringReflection :: GameState -> GameState
        neighbouringReflection g = updateEntities (\(MkEntity p v) -> let e = (MkEntity p v)
                                                    in if any (\x -> (pos x == pos (moveEntity e)) && (vel x == neg v)) (entities g)
                                                       then MkEntity p (neg v)
                                                       else e) g

        hitSounds :: GameState -> GameState -> GameState
        hitSounds g g' = if currentLevel g /= currentLevel g' then g'
                         else if isLeft (clone g) /= isLeft (clone g')
                              then queueSound 7 g'
                              else if (count stationary (entities g) - count stationary (entities g')) > (length (entities g') - length (entities g))
                                   then queueSound 2 g'
                                   else if map vel (boxes g) /= map vel (boxes g')
                                        then queueSound 1 g'
                                        else if (vel (player g) /= vel (player g')) || (fmap vel (clone g) /= fmap vel (clone g'))
                                             then queueSound 0 g'
                                             else g'
          where count p = length . filter p
                stationary = (==(0,0)) . vel


        

eventHandler :: Event -> GameState -> GameState
eventHandler (Event _ (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym s _ _)))) g
  = case state g of
      OverWorld     -> overWorldEvent
      LevelTrans    -> g {state = OverWorld}
      TitleScreen m -> g
      Ending        -> g {state = TitleScreen (men (textFont g))}
 where overWorldEvent 
         | (s `elem` [ScancodeW, ScancodeUp])    = walk (0, -1) g
         | (s `elem` [ScancodeA, ScancodeLeft])  = walk (-1, 0) g
         | (s `elem` [ScancodeS, ScancodeDown])  = walk (0,  1) g
         | (s `elem` [ScancodeD, ScancodeRight]) = walk (1,  0) g
         | (s `elem` [ScancodeZ])                = tryUndo g
         | otherwise      = g
eventHandler e g = case state g of
                     TitleScreen m -> eventHandleMenu (const m) (\m' s -> s {state = TitleScreen m'})
                                                      (\() s -> s {levelTransDelay = 1}) (textFont g) e g
                     _             -> g


animDelay = 10

loadAnim :: FilePath -> RenderingContext Animation
loadAnim f = mapM (load . ((f ++ "/") ++)) =<< fmap sort (liftIO (listDirectory f))
  
loadTextures :: RenderingContext [Animation]
loadTextures =
  do conts <- liftIO $ B.readFile "Level/MetaData/tiles"
     let tileConfs = fromJust $ decodeStrict conts :: [TileConfig]
     texts <- mapM (loadAnim . ("Art/" ++) . filepath) tileConfs
     plain <- mapM (fmap singleton . load . ("Art/" ++)) ["ending.bmp", "title.bmp", "levelTrans.bmp", "arrow.bmp"]
     return (texts ++ plain)

loadSounds :: IO [Chunk]
loadSounds = mapM (\(filename, vol) -> M.load ("Sounds/" ++ filename) >>= (\c -> setVolume vol c >> return c))
             [("light_hit.wav", 64), ("medium_hit.wav", 50), ("hard_hit.wav", 64)
             , ("egg1.wav", 64), ("egg2.wav", 64), ("egg3.wav", 64), ("goodlax.wav", 64),("laxeggcrack.wav", 64)
             , ("new watersound.wav", 64),("walkoneggtesteww.wav", 64),("yaylax.wav", 64), ("laxmusik.wav", 32), ("laxtitle.wav", 32), ("laxending.wav", 32)]

--loadMusic :: IO [Music]
--loadMusic = mapM (M.load . ("Sounds/" ++)) []


men :: Font -> Menu ()
men f = dir' (color orange $ vtextbutton p "start" ()
     <>   (color orange $ mtextbutton (p + offset 2) "credit" (dir f (Just (black, 5)) (dark blue) 5 credits)))
  where credits = (color orange $ mtextbutton  p "back" (men f))
               <> noInterText (v + offset 1) "pavenpaven (Mattias Burman): programmer"
               <> noInterText (v + offset 2) "fish: art, level design, sound design, fish"
               <> noInterText (v + offset 3) "chiminiio: tile art"
               <> noInterText (v + offset 4) "font from Daniel Linssen (check out itch.io)"
               <> noInterText (v + offset 6) "All art is original"
               <> noInterText (v + offset 7) "Written in haskell with SDL2"
               <> noInterText (v + offset 8) "Code licence is GPL3"
               
        p = (V2 (5*tileWidth + 40) 300)
        v = (V2 (1*tileWidth - 20) 300)
        offset i = V2 0 (50 * i)

        dir' = dir f (Just (black, 3)) (dark blue) 3

initialGame :: RenderingContext GameState
initialGame =
  do args   <- liftIO getArgs
     --let levl = if null args then "level0" else head args
     textrs <- loadTextures
     f      <- F.load fontFilePath fontPointSize
     sounds <- liftIO loadSounds
     return (MkGameState textrs sounds (take (length sounds) $ repeat False) (-1, -1) f waterTileMap (TitleScreen (men f)) (Left ((-1,-1), False)) [] [] (MkEntity (3,2) (0, 0)) [] 0 0 0) 
     
waterTileMap = [[1 | i <- [1..13]] | j <- [1..12]]
          

main :: IO ()
main = runGameIO (defaultConfig {windowConf = windowConfig {windowInitialSize = V2 (fromIntegral screenWidth) (fromIntegral screenHeight)}})
                 initialGame
                 draw
                 update
                 eventHandler
              
