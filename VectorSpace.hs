{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module VectorSpace where

class VectorSpace v where
  type (Scalar v) :: *
  zeroV :: v
  negateV :: v -> v 
  (|+|) :: v -> v -> v
  (|-|) :: v -> v -> v  
  (*|) :: Scalar v -> v -> v
  (|*) :: v -> Scalar v -> v
  (|/) :: Fractional (Scalar v) => v -> Scalar v -> v
  (|.|) :: v -> v -> Scalar v
  v |* s = s *| v
  v |/ s = (recip s) *| v
  v1 |-| v2 = v1 |+| (negateV v2)
  negateV v = zeroV |-| v

infixl 6 |+|
infixl 6 |-|
infixl 7 *|
infixl 7 |*
infixl 7 |/
infixl 8 |.|

norm :: (VectorSpace a, Floating (Scalar a)) => a -> Scalar a
norm x = sqrt (x |.| x)

unit :: (VectorSpace a, Floating (Scalar a)) => a -> a
unit x = x |/ (norm x)

lerp :: VectorSpace a => a -> a -> Scalar a -> a
lerp v1 v2 p = v1 |+| (p *| (v2 |-| v1))

instance VectorSpace Double where
  type Scalar Double = Double
  zeroV = 0
  negateV x = -x
  (|+|) = (+)
  (*|) = (*)
  (|.|) = (*)
 
