{-# LANGUAGE RecursiveDo #-}

module Widgets 
  ( centreText
  
  , renderButton
  , button
  
  , toggleButton
  ) where

import Reflex
import Reflex.Gloss.Scene
import Graphics.Gloss



smallText :: String -> Picture
smallText str = scale 0.1 0.1 $ text str

centreText :: String -> Picture
centreText str = translate (-x) (-y) $ smallText str where
  x = fromIntegral (length str) * charWidth * 0.5
  y = charHeight * 0.5
  
  (charWidth, charHeight) = (6, 10)
  
renderButton :: Color -> Bool -> Bool -> String -> Vector -> Picture
renderButton c isHovering isPressing str (sx, sy) = mconcat 
    [ color fill $ rectangleSolid sx sy
    , centreText str
    , if isHovering then rectangleWire sx sy else mempty
    ] where
    
    fill = if isHovering && isPressing then dark c else c 


coloredButton :: Reflex t => Behavior t Color -> Behavior t String -> Behavior t Vector -> Scene t (Event t ())
coloredButton col str size = do
  
  target <- targetRect size
  (click, pressing) <- clicked LeftButton target
  
  render $ renderButton <$> col <*> hovering target
            <*> pressing <*> str <*> size
  return click
  

button :: Reflex t => Behavior t String -> Behavior t Vector -> Scene t (Event t ())
button = coloredButton (pure $ light azure)
  
  
toggleButton :: Reflex t => Behavior t String -> Behavior t Vector -> Event t Bool -> Scene t (Dynamic t Bool, Event t Bool)
toggleButton str size setter = do
  rec 
    click <- coloredButton (currentColor <$> current down) str size
    let clickDown = tag (not <$> current down) click
    
    down <- holdDyn False $ leftmost [clickDown, setter]
  
  return (down, clickDown)

  where
    currentColor d = if d then orange else light azure
  