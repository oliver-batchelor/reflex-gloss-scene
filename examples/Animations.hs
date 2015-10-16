-- {-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
-- import Reflex.Monad.Time
-- import Reflex.Animation


-- import Reflex.Monad

import Graphics.Gloss



-- import Control.Applicative hiding (optional)

import Widgets

    

-- data Track time = Rotation Float | Position Vector deriving Ord
-- 
-- 
-- newtype Tracks time = Tracks { unTracks :: [Track time] }

--     
--     
--     
--     
-- bounce :: PictureAnimation
-- bounce = repeat (half (sine 0.5))
  
  
  
additive :: Reflex t => Scene t ()
additive = do
  
  translation (-60, 0) $ do
    (_, bounceClick) <- translation (0, 80) $ toggleButton (pure "Bounce") (pure (80, 30)) never
    (_, wiggleClick) <- translation (0, 30) $ toggleButton (pure "Wiggle") (pure (80, 30)) never
    
    return ()
    
  translation (60, 0) $ 
    render $ square <$> pure green
  
  
  where
    
    square c = color c $ rectangleSolid 40 40

 

widget :: Reflex t => Scene t  ()
widget = do
  additive
  
  return ()



main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Animations example" (600, 600) (0, 0)
    background = white
    frequency = 30