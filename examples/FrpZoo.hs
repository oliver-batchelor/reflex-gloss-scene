{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene

import Graphics.Gloss
import Data.Monoid
import Data.Maybe
import Data.Bifunctor

import Control.Applicative hiding (optional)
import Widgets

foldMerge :: MonadReflex t m => a -> [Event t (a -> a)] -> m (Dynamic t a)
foldMerge initial events = foldDyn ($) initial (mergeWith (.) events) 

buttonPair :: (Reflex t) => Behavior t Int -> Scene t (Event t (), Event t ())
buttonPair b = mdo
  inc    <- translation (0, 20) $ button (show <$> b) (pure (80, 30))
  toggle <- translation (0, -20) $ button (pure "Toggle") (pure (80, 30))
  return (inc, toggle) 

resetButtons :: (Reflex t) => Scene t ()
resetButtons = mdo
  (inc, toggle) <- buttonPair (fromMaybe (-1) <$> current d) 
  d <- foldMerge (Just 0) [fmap (+1) <$  inc, reset <$ toggle] 
  return ()
    where
      reset Nothing  = Just 0
      reset (Just n) = Nothing  
  
toggleButtons :: (Reflex t) => Scene t ()
toggleButtons = do
  rec (inc, toggle) <- buttonPair ((\(n, b) -> if b then n else (-1)) <$> current d) 
      d <- foldMerge (0, True) [first (+1) <$ inc, second not <$ toggle]
  return ()
  
widget :: Reflex t => Scene t  ()
widget = do
  translation (-100, 0) $ resetButtons 
  translation (0, 0) $ toggleButtons 
  translation (100, 0) $ toggleButtons 


main = playSceneGraph display background frequency widget
  where 
    display = (InWindow "reflex frp-zoo" (300, 200) (0, 0))
    background = white
    frequency = 60  