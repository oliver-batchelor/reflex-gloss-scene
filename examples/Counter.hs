{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Monad.Time
import Reflex.Animation


import Reflex.Monad

import Graphics.Gloss

import Widgets


counter :: (MonadReflex t m) => Event t a -> m (Behavior t Int)
counter e = mdo
  n <- hold 0 next
  
  let next = attachWith (+) n (1 <$ e)  
  -- (1 <$ e) = fmap (const 1) e
  return n

    
counterButton :: Reflex t =>  Scene t ()
counterButton  = do
  click <- translation (0, 30) $ button (pure "Inc") (pure (80, 30)) 
  count <- counter click

  translation (0, -30) $ render (showNumber <$> count)
  return ()
  
  where 
    showNumber x = scale 0.2 0.2 $ text (show x)
  

  
  
resetCounter :: (MonadReflex t m) => Event t a -> Event t a -> m (Behavior t Int)
resetCounter e reset = mdo
  n <- hold 0 $ leftmost [ 0 <$ reset, next ] 
  
  let next = attachWith (+) n (1 <$ e)  
  -- (1 <$ e) = fmap (const 1) e
  return n

    
resetButton :: Reflex t =>  Scene t ()
resetButton  = do
  click <- translation (0, 30) $ button (pure "Inc") (pure (80, 30)) 
  reset <- translation (0, 90) $ button (pure "Reset") (pure (80, 30)) 
  
  count <- resetCounter2 click reset

  translation (0, -30) $ render (showNumber <$> count)
  return ()
  
  where 
    showNumber x = scale 0.2 0.2 $ text (show x)
  
  
 
resetCounter2 :: (MonadReflex t m) => Event t a -> Event t a -> m (Behavior t Int)
resetCounter2 e reset = do
  d <- foldDyn ($) 0 $ mergeWith (.)  
    [ (+1)    <$ e  
    , const 0 <$ reset 
    ]
  
  return (current d) 
 
 
 

widget :: Reflex t => Scene t  ()
widget = do 
  translation (-100, 0) $  counterButton
  translation (100, 0) $  resetButton


main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Counter" (600, 600) (0, 0)
    background = white
    frequency = 30