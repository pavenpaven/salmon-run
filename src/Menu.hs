module Menu where

import Game

import SDL      hiding (trace)
import SDL.Font hiding (Color)

import Data.Maybe
import Data.List (findIndex, intercalate, reverse)

import Control.Monad
import Control.Monad.IO.Class

type Pos = V2 Int
type Tex = Int -- Indexing into texture list


data Sprite = Image Rect Tex
            | Label (V2 Int) Color String
            | Rectang Color Rect

bounds :: Font -> Sprite -> Rect
bounds f (Image r _)   = r
bounds f (Label p _ s) = let (x,y) = textSize f s in (p,  V2 x y)
bounds f (Rectang _ r) = r

data Widget a = MButton Sprite (Menu a)
              | VButton Sprite a
              | NoInter Sprite

data ButtonConf = MkButtonConf {outline      :: Int
                               ,outlineColor :: Color
                               ,background   :: Maybe Color
                               ,padding      :: Either Rect Int
                               ,textColor    :: Color}
                  deriving (Eq, Show)

wSprite :: Widget a -> Sprite
wSprite (MButton s _) = s
wSprite (VButton s _) = s
wSprite (NoInter s)   = s

newSprite :: Sprite -> Widget a -> Widget a
newSprite s (MButton _ a) = MButton s a 
newSprite s (VButton _ a) = VButton s a
newSprite s (NoInter _)   = NoInter s

updateSprite :: (Sprite -> Sprite) -> Widget a -> Widget a
updateSprite f w = newSprite (f (wSprite w)) w

type Menu a = [Widget a]

instance Functor Widget where
  fmap f (MButton s m) = MButton s $ map (fmap f) m
  fmap f (VButton s a) = VButton s $ f a
  fmap f (NoInter s)   = NoInter s

drawMenu :: Font -> [Texture] -> Menu a -> GraphicalProcess
drawMenu f ts m = mconcat $ map (drawSprite f ts . wSprite) m

drawSprite :: Font -> [Texture] -> Sprite -> GraphicalProcess
drawSprite f ts (Image r t) = display texture r
  where texture  = ts !! t
drawSprite f ts (Label p c s) = text f c p s
drawSprite f ts (Rectang c r) = rect c r

inside :: Pos -> Rect -> Bool
inside (V2 x y) (V2 a b, V2 c d) = (a <= x) && (x <= (a + c)) && (b <= y) && (y <= (b + d))

eventHandleMenu :: (state -> Menu b) -> (Menu b -> state -> state) -> (b -> state -> state) -> Font -> Event -> state -> state
eventHandleMenu getMenu setMenu handleMenu f (Event _ (MouseButtonEvent (MouseButtonEventData _ Pressed _ button _ (P (V2 x' y'))))) g
  = let pos = V2 (fromIntegral x') (fromIntegral y')
    in case button of
          ButtonLeft -> fromMaybe g $ fmap upd $ listToMaybe $ reverse $ filter (\x -> interactable x && (pos `inside` bounds f (wSprite x))) $ getMenu g
          _           -> g
  where interactable (MButton _ _) = True
        interactable (VButton _ _) = True
        interactable (NoInter s)   = False

        upd (MButton _ m) = setMenu m g
        upd (VButton _ v) = handleMenu v g
        upd a             = g
eventHandleMenu getMenu setMenu handleMenu f _ g = g

dir :: Font -> Maybe (Color, Int) -> Color -> Int -> Menu a -> Menu a
dir f out c margin m = fromMaybe mempty (fmap (\(c', i) -> [NoInter $ Rectang c' (widen (i + margin) area)]) $ out)
                    <> [NoInter $ Rectang c (widen margin area)]
                    <> m
  where area = foldl rectCoProduct (head rects) rects

        rects = map (bounds f . wSprite) m

        widen :: Int -> Rect -> Rect
        widen i (V2 x y, V2 w h) = (V2 (x - i) (y - i), V2 (w + 2*i) (h + 2*i))
        
        rectCoProduct :: Rect -> Rect -> Rect
        rectCoProduct (V2 x y, V2 w h) (V2 x' y', V2 w' h')
          = let (x'', y'') = ((min x x'), (min y y'))
            in (V2 x'' y'', V2 (max (x + w) (x' + w') - x'')
                               (max (y + h) (y' + h') - y''))

color :: Color -> Menu a -> Menu a
color c = map colorW
  where colorW w = updateSprite colorS w

        colorS (Label p _ s) = Label p c s
        colorS (Rectang _ r) = Rectang c r
        colorS a             = a

vtextbutton :: Pos -> String -> a -> Menu a
vtextbutton p s a = [VButton (Label p white s) a]

mtextbutton :: Pos -> String -> Menu a -> Menu a
mtextbutton p s m = [MButton (Label p white s) m]

noInterText :: Pos -> String -> Menu a
noInterText p s = [NoInter (Label p white s)]

button :: ButtonConf -> String -> Menu a
button = undefined
