{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Signal where

import Control.Applicative
import Data.Functor

import VectorSpace
import Linear

-- maybe this is easier to implement

data Signal a =
  SigC a (T -> Signal a) |
  SigD a T (Signal a) |
  SigE a

instance Functor Signal where
  fmap f (SigC x g) = SigC (f x) (\dt -> fmap f (g dt))
  fmap f (SigD x d next) = SigD (f x) d (fmap f next)
  fmap f (SigE x) = SigE (f x)

instance Applicative Signal where
  pure x = SigE x
  SigC f ff <*> SigC x xx = SigC (f x) (\dt -> (ff dt) <*> (xx dt)) 
  SigC f ff <*> s2@(SigD x d xx) = SigC (f x) next where
    next dt = ff dt <*> dropS dt s2
  SigC f ff <*> s2@(SigE x) = SigC (f x) (\dt -> (ff dt) <*> s2)

  s1@(SigD f d1 ff) <*> SigC x xx = SigC (f x) next where
    next dt = dropS dt s1 <*> xx dt
  s1@(SigD f d1 ff) <*> s2@(SigD x d2 xx) = SigD (f x) d3 next where
    d3 = min d1 d2
    next = (dropS d3 s1) <*> (dropS d3 s2)
  SigD f d ff <*> s2@(SigE x) = SigD (f x) d (ff<*>s2) where

  (SigE f) <*> (SigE x) = SigE (f x)
  s1@(SigE f) <*> (SigD x d xx) = SigD (f x) d (s1 <*> xx)
  s1@(SigE f) <*> (SigC x xx) = SigC (f x) (\dt -> s1 <*> (xx dt))

instance (VectorSpace a, Scalar a ~ T) => VectorSpace (Signal a) where
  type Scalar (Signal a) = Signal (Scalar a)
  zeroV = pure zeroV
  negateV = fmap negateV 
  s1 |+| s2 = (|+|) <$> s1 <*> s2
  r *| s = (*|) <$> r <*> s
  s1 |.| s2 = (|.|) <$> s1 <*> s2

(<:>) :: Signal a -> Signal [a] -> Signal [a]
x <:> xs = (:) <$> x <*> xs

sconcat :: [Signal a] -> Signal [a]
sconcat = foldr (<:>) (pure [])
  
dropS :: T -> Signal a -> Signal a  
dropS _ s@(SigE _) = s
dropS dt (SigD x d next) | dt <= d = SigD x (d-dt) next -- < or == so dt=d stays here
                         | otherwise = dropS (dt-d) next
dropS dt (SigC x xx) = xx dt

current :: Signal a -> a
current (SigC x _) = x
current (SigD x _ _) = x
current (SigE x) = x

after :: Signal a -> T -> Signal a -> Signal a
after (SigE x) t s2 = SigD x t s2
after (SigD x d xx) t s2 = hmm where
  hmm | t < d = SigD x (d-t) s2
      | otherwise = SigD x d (after xx (t-d) s2)
after (SigC x xx) t s2 = SigC x next where
  next dt | dt < t = after (xx dt) (t-dt) s2
          | otherwise = dropS (dt-t) s2


instantly :: a -> Signal a -> Signal a
instantly x s = SigD x 0 s

integral :: (VectorSpace a, Scalar a ~ T) => a -> Signal a -> Signal a
integral x0 s@(SigE v) = SigC x0 next where
  next dt = integral (x1 dt) s
  x1 dt = x0 |+| v|*dt

integral x0 s@(SigD v d next) = SigC x0 next where
  next dt = integral (x1 dt) (dropS dt s)
  x1 dt = x0 |+| v|*dt

integral x0 s@(SigC v vv) = SigC x0 next where
  x1 dt = x0 |+| v|*dt
  next dt = integral (x1 dt) (vv dt)

ray x v = integral x (pure v)
