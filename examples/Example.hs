{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Monad.Time
import Reflex.Animation


import Reflex.Monad

import Graphics.Gloss




import Widgets

    

pictureButton :: Reflex t => Behavior t Picture -> Scene t (Event t (Behavior t Picture))
pictureButton picture = do
  translation (0, -30) $ render picture
  click <- translation (0, 30) $ button (pure "Select") (pure (80, 30)) 
  return (picture  <$ click)
  
  
channels :: Reflex t => Scene t (Event t (Behavior t Picture))
channels = do
  
  time <- getTime
  
    
  translation (-100, 0) $ pictureButton (square <$> pure green)
  translation (0, 0) $ pictureButton (square <$> pure green)
  translation (100, 0) $ pictureButton (square <$> pure green)
  
  
  where
    
    square c = color c $ rectangleSolid 40 40

 

widget :: Reflex t => Scene t  ()
widget = do
  channels
  
  return ()



main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Animations example" (600, 600) (0, 0)
    background = white
    frequency = 30