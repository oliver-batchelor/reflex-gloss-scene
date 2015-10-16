{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reflex.Gloss.Vector 
  ( mv3
  , mm3
  , rotationMat
  , identityMat
  , translationMat
  , transformPoint
  
  , Matrix33, Vector3
  , module Data.VectorSpace
  
  ) where
  
  
import Graphics.Gloss.Rendering

import Data.VectorSpace


type Vector3 = (Float, Float, Float)
type Matrix33 = (Vector3, Vector3, Vector3)


mv3 :: Matrix33 -> Vector3 -> Vector3
mv3  (a, b, c) v = (a <.> v, b <.> v, c <.> v)


vm3 :: Vector3 -> Matrix33 -> Vector3
vm3 (x, y, z) (a, b, c) = x *^ a ^+^ y *^ b ^+^ z *^ c



mm3 :: Matrix33 -> Matrix33 -> Matrix33
mm3 (a, b, c) m = (a `vm3` m, b `vm3` m, c `vm3` m) 


rotationMat :: Float -> Matrix33
rotationMat deg = 
 ( (ca, -sa,   0)
 , (sa,  ca,   0)
 , (0,    0,   1))
  
  where
    a = deg * pi / 180.0 
    ca = cos a
    sa = sin a
    
    
identityMat ::  Matrix33
identityMat = 
  ( (1,    0,   0)
  , (0,    1,   0)
  , (0,    0,   1)) 

    
translationMat :: Vector -> Matrix33
translationMat (dx, dy) =  
  ( (1,    0,  dx)
  , (0,    1,  dy)
  , (0,    0,  1)) 
  
                                   

transformPoint :: Matrix33 -> Point -> Point
transformPoint transform (x, y) = (x', y') ^* (1 / w) where
  (x', y', w) = transform `mv3` (x, y, 1)  
  
  
instance AdditiveGroup Color where
    zeroV = makeColor 0 0 0 0
    c ^+^ c' = makeRawColor (r + r') (g + g') (b + b') (a + a')
      where  (r, g, b, a) = rgbaOfColor c
             (r', g', b', a') = rgbaOfColor c'
    negateV c = makeRawColor (-r) (-g) (-b) (-a)
      where (r, g, b, a) = rgbaOfColor c


instance VectorSpace Color where
    type Scalar Color = Float
  
    s *^ c  = makeRawColor (r * s) (g * s) (b * s) (a * s) 
      where (r, g, b, a) = rgbaOfColor c
  