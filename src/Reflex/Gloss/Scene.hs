{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances  #-}


-- | Reflex.Gloss.Scene
-- A simple scene graph implementation rendered using gloss. Contains functionality for picking and transformations.
-- Implements interfaces for MonadSwitch (see reflex-transformers) and MonadTime (see reflex-animations) for switching, and animation support respectively.


module Reflex.Gloss.Scene
  ( playSceneGraph
  , rotation
  , activeRotation
  
  , translation
  , activeTranslation
  
  , render
  , Scene
  
  , Time
  
  , targetRect
  , targetCircle
  , makeTarget
  , hovering
  , Target
   
  , mouseUp
  , mouseDown
  , clicked
  
  , mouseOver
  
  , MouseButton(..)
  , SpecialKey (..)
  , Input (..)
  
  , module Reflex.Monad
  
  )
  where

import Control.Monad.Fix      
import Control.Monad.Reader

import Control.Lens hiding ((<.>), transform)
import Control.Applicative

import Data.Monoid
import Data.List
import Data.List.NonEmpty (nonEmpty)


import Reflex
import Reflex.Gloss

import Reflex.Monad.Time

import Graphics.Gloss.Interface.IO.Game hiding (Event, KeyDown, KeyUp)
import qualified Graphics.Gloss.Interface.IO.Game as G


import Reflex.Monad
import Reflex.Monad.ReaderWriter
import Reflex.Monad.ReflexM

import Reflex.Gloss.Vector

  
data Input = MouseDown MouseButton 
           | MouseUp MouseButton
           | CharDown Char
           | CharUp Char
           | SpecialUp SpecialKey
           | SpecialDown SpecialKey  
           | MouseOver 
           | MouseOut
  deriving (Show, Eq, Ord)
        
type Time = Float
        
data SceneNode t = SceneNode 
  { _sceneTime          :: Behavior t Time 
  , _sceneTick          :: Event t Time
  , _sceneMousePosition :: Behavior t Vector
  , _sceneTransform     :: Behavior t Matrix33
  , _sceneEvents        :: Event t Input
  , _sceneWindowSize      :: Behavior t Vector
  }
  
$(makeLenses ''SceneNode)
$(makePrisms ''Input)


$(makePrisms ''G.Event)
$(makePrisms ''G.Key)


newtype Scene t a = Scene { runScene :: ReaderWriterT (SceneNode t) [Behavior t Picture] (ReflexM t) a } 
                        deriving (Monad, Functor, Applicative, MonadFix)


deriving instance Reflex t => MonadSample t (Scene t)
deriving instance Reflex t => MonadHold t (Scene t)
deriving instance Reflex t => MonadWriter [Behavior t Picture] (Scene t)
deriving instance Reflex t => MonadReader (SceneNode t) (Scene t)

  
 
 
instance Reflex t => MonadSwitch t (Scene t) where
  switchM     u  =  Scene $ switchM (runScene <$> u)
  switchMapM  um =  Scene $ switchMapM (runScene <$> um)


  
  
toVector :: Integral a => (a, a) -> Vector
toVector (a, b) = (fromIntegral a, fromIntegral b)


toInput :: (Key, KeyState, a, b) -> Input
toInput (key, d, _, _) = case (key, d) of                       
    (MouseButton b, Down) -> MouseDown b
    (MouseButton b, Up)   -> MouseUp b
    
    (Char c, Down) -> CharDown c
    (Char c, Up)   -> CharUp c
    
    (SpecialKey k, Down) -> SpecialDown k
    (SpecialKey k, Up)   -> SpecialUp k

    
    
            
previewF ::  FunctorMaybe f => Getting (First b) s b -> f s -> f b
previewF getter = fmapMaybe (preview getter) 
            
        
runSceneGraph :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => Vector -> Scene t () -> Event t Float -> Event t InputEvent -> m (Behavior t Picture)
runSceneGraph initialSize scene tick inputs = do
  time <- foldDyn (+) 0 tick  
  mousePosition <- hold (1/0, 1/0) $ previewF _EventMotion inputs
  windowSize <- hold initialSize (toVector <$> previewF _EventResize inputs)
  
  let rootNode = SceneNode 
        { _sceneTime          = current time
        , _sceneTick          = tick
        , _sceneMousePosition   = mousePosition
        , _sceneEvents = toInput <$> previewF _EventKey inputs 
        , _sceneTransform = constant identityMat 
        , _sceneWindowSize = windowSize
        }
  
  runSceneGraph' rootNode scene

      
  
runSceneGraph' :: (Reflex t, MonadHold t m, MonadFix m) => SceneNode t -> Scene t a -> m (Behavior t Picture)
runSceneGraph' node = fmap mconcat . runReflexM . flip execReaderWriterT node . runScene


-- | Run a scenegraph in IO
playSceneGraph :: Display -> Color -> Int ->  (forall t. Reflex t => Scene t  ()) -> IO ()
playSceneGraph display col frequency scene = playReflex display col frequency (runSceneGraph (displaySize display) scene) where
  displaySize (InWindow _ s _) = toVector s
  displaySize (FullScreen s) = toVector s
  
  
-- | Render a picture in the current widget
render :: Reflex t => Behavior t Picture -> Scene t ()
render r = tell [r]

    
    
-- | Local mouse position relative to the widget
localMouse :: Reflex t => Scene t (Behavior t Point)
localMouse = asks sceneLocalMouse


-- | Get the global mouse position in the window
globalMouse :: Reflex t => Scene t (Behavior t Point)
globalMouse = asks _sceneMousePosition

-- | Rotate a child widget using a constant
rotation :: Reflex t => Float -> Scene t a -> Scene t a 
rotation degrees child = localRender (fmap $ G.Rotate degrees) $ 
    localTransform (rotationMat degrees) child

-- | Rotate a child widget actively using a Behavior
activeRotation :: Reflex t => Behavior t Float -> Scene t a -> Scene t a 
activeRotation degrees child = localRender (G.Rotate <$> degrees <*>) $   
    localTransformDyn (rotationMat <$> degrees) child
  
-- | Translate a child widget using a constant
translation :: Reflex t =>  Vector -> Scene t a -> Scene t a 
translation (x, y) child = localRender (fmap $ G.Translate x y) $ 
    localTransform (translationMat (-x, -y)) child

-- | Translate a child widget actively using a Behavior
activeTranslation :: Reflex t => Behavior t Vector -> Scene t a -> Scene t a 
activeTranslation v child = localRender (uncurry G.Translate <$> v <*>) $   
    localTransformDyn (translationMat . negate <$> v) child  

  
-- | Type for picking targets
data Target t = Target 
  { targetNode  :: SceneNode t   
  , hovering    :: Behavior t Bool
  }  
  
  
  
-- | Make a target from an intersection function  
makeTarget :: Reflex t => Behavior t (Vector -> Bool) -> Scene t (Target t) 
makeTarget intersects = do
  node  <- ask
  mouse <- localMouse
  return  Target 
    { targetNode = node
    , hovering   = intersects <*> mouse
    }  
  

intersectRect :: Vector -> Vector -> Bool
intersectRect (sizeX, sizeY) (x, y) = x >= -sx && x <= sx && y >= -sy && y <= sy  where
    sx = sizeX * 0.5
    sy = sizeY * 0.5

intersectCircle :: Float -> Vector -> Bool
intersectCircle radius (x, y) = x * x + y * y < radius * radius    
    
    
-- | Make a circular picking target    
targetCircle :: Reflex t => Behavior t Float -> Scene t (Target t) 
targetCircle size = makeTarget (intersectCircle <$> size)


-- | Make a rectangular picking target    
targetRect :: Reflex t => Behavior t Vector -> Scene t (Target t) 
targetRect size = makeTarget (intersectRect <$> size)


-- Match a particular kind of mouse or key event
nodeEvent :: Reflex t => Input -> SceneNode t -> Event t ()
nodeEvent input node = match input $ node ^. sceneEvents  


-- | Mouse down event on a picking target
mouseDown :: Reflex t => MouseButton -> Target t -> Event t ()
mouseDown button target = gate (hovering target) $ nodeEvent (MouseDown button) (targetNode target)
 
-- | Mouse up event on a picking target
mouseUp :: Reflex t => MouseButton -> Target t -> Event t ()
mouseUp button target = gate (hovering target) $ nodeEvent (MouseUp button) (targetNode target)


-- | Mouse over event on a picking target, returns events for (entering, leaving)
mouseOver :: (Reflex t, MonadTime t time m) => Target t -> m (Event t (), Event t ())
mouseOver target = do
  ov <- observeChanges (hovering target)
  return (match True ov, match False ov)

-- | Mouse click event for picking target, returns click event and behavior indicating current clicking
clicked :: (Reflex t, MonadHold t m) => MouseButton -> Target t -> m (Event t (), Behavior t Bool)
clicked button target = do
  pressing <- hold False $ leftmost 
    [ False <$ nodeEvent (MouseUp button) (targetNode target)
    , True <$ mouseDown button target  
    ]
    
  return (void $ gate pressing (mouseUp button target), pressing)

getTick :: Reflex t => Scene t (Event t Float)
getTick = asks _sceneTick



instance Reflex t => MonadTime t Time (Scene t) where

  --observe :: MonadTime t m => Behavior t a -> m (Event t a)
  observe b = tag b <$> getTick  

  --integrate :: (VectorSpace v, Scalar v ~ Time, MonadReflex t m) => v -> Behavior t v -> Scene t (Behavior t v)
  integrate x dx  = do
    frame <- attachWith (^*) dx <$> getTick
    current <$> foldDyn (^+^) x frame
    

  --after :: MonadReflex t m => Time -> Scene t (Event t ())
  after t = do
    time <- foldDyn (+) 0 =<< getTick
    onceE $ matchBy (>= t) (updated time)
    
    
  getTime = asks _sceneTime
    
    
  -- | Delay an Event by a time period
  --delay :: MonadReflex t m => Event t (a, Time) ->  Scene t (Event t [a])
  delay e = do
    tick  <- asks _sceneTick
    time  <- getTime

    rec
      b <- hold [] waiting
    
      let next = leftmost [new, tag b tick]
          (waiting, firing) = split $ 
            attachWith (\t -> partition ((> t) . snd)) time next

          new = pushFor e $ \(a, p) -> do
            now <- sample time
            existing <- sample b
            return ((a, now + p) : existing)
            
    return (fmapMaybe (nonEmpty . fmap fst) firing)  


    
-- Internal utility functions for Scene Node transformations
localRender :: Reflex t => (Behavior t Picture -> Behavior t Picture) -> Scene t a -> Scene t a 
localRender f = censor (pure . f . mconcat) 


transformNode :: (Reflex t) => Matrix33 -> SceneNode t -> SceneNode t
transformNode transform node = node & sceneTransform %~ fmap (transform `mm3`)

  
transformNodeDyn :: (Reflex t) => Behavior t Matrix33 -> SceneNode t -> SceneNode t
transformNodeDyn transform node = node & sceneTransform %~ liftA2 (flip mm3) transform
  
localTransform :: Reflex t => Matrix33 -> Scene t a -> Scene t a 
localTransform transform = local (transformNode transform)
   
    
localTransformDyn :: Reflex t => Behavior t Matrix33 -> Scene t a -> Scene t a 
localTransformDyn transform = local (transformNodeDyn transform)
    
sceneLocalMouse :: Reflex t => SceneNode t -> Behavior t Point
sceneLocalMouse node = liftA2 transformPoint (_sceneTransform node) (_sceneMousePosition node)    