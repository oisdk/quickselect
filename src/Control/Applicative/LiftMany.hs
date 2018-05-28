module Control.Applicative.LiftMany
  (liftA2
  ,liftA3
  ,liftA4
  ,liftA5)
  where

import Control.Applicative

{-# INLINE liftA4 #-}
liftA4
    :: Applicative f
    => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f w x y z = liftA3 f w x y <*> z

{-# INLINE liftA5 #-}
liftA5
    :: Applicative f
    => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f v w x y z = liftA3 f v w x <*> y <*> z
