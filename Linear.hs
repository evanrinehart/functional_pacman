{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Linear (T,R,R2) where

import VectorSpace
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.DrawingCombinators.Affine

type T = R

instance VectorSpace R2 where
  type Scalar R2 = R
  zeroV = (0,0)
  negateV (x,y) = (-x, -y)
  (x1,y1) |+| (x2,y2) = (x1+x2, y1+y2)
  r *| (x,y) = (r*x, r*y)
  (x1,y1) |.| (x2,y2) = x1*x2 + y1*y2

instance VectorSpace R where
  type Scalar R = R
  zeroV = 0
  negateV = negate
  (|+|) = (+)
  (*|) = (*)
  (|.|) = (*)
