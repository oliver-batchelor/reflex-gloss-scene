{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Gloss.Random
import Reflex.Monad.Time
import Reflex.Animation

import Graphics.Gloss

import Data.Monoid
import Data.Maybe

import qualified Data.Map as Map

import Control.Applicative hiding (optional)

import Widgets

    
    
      
randomCircle :: RandomGen g => Rand g (Color, Point, Float) 
randomCircle = do
  c <- getRandomR (makeColor 0 0 0 1, makeColor 1 1 1 1)
  p <- getRandomR ((-200, -200), (200, 200))
  r <- getRandomR (40, 100)
  return (c, p, r)

  
fadeFrom :: Color -> Clip Time Color
fadeFrom from = keyframes from [(0.5, withAlpha 0 from)]

flashRed :: Color -> Clip Time Color
flashRed c = keyframes c [(0.4, red), (0.4, c)]


makeCircle :: (Reflex t) => (Color, Point, Float) ->  Scene t ((), Event t ())  
makeCircle (c, p, radius) = translation p $ do 
  e <- chain (Chain active >-> Chain quitting) ()
  return ((), e)
  
  where
  
    quitting currentCol = do
      (col, done) <- playClamp (fadeFrom currentCol)
      renderCircle col
      return done

    active () =  do
      target <- targetCircle (constant radius)
      (rightClick, _) <- clicked RightButton target
      
      (enter, leave) <- mouseOver target
      rec 
        (flashing, _) <- playOn (gate (isNothing <$> flashing) $ flashRed c <$ enter)

      let col = fromMaybe c <$> flashing
      renderCircle col

      return (tag col rightClick)
    
    renderCircle currentCol = render (renderBody <$> currentCol)    
    renderBody c = color c $ circleSolid radius    
    
    
    

 

widget :: Reflex t => Scene t ()
widget = do
  
  click <- translation (200, 200) $ 
           rotation 40 $
      button (constant "New") (constant (80, 30))
  
  degrees <- current <$> integrate 0 (constant 4)

  activeRotation degrees $ do
    newItems <- fmap makeCircle <$> foldRand (mkStdGen 0) randomCircle click  
    circles <- holdMapDyn =<< collection [] (pure <$> newItems) 
    render $ centreText . show . Map.size <$> current circles


  return ()



main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Collections example" (600, 600) (0, 0)
    background = white
    frequency = 30