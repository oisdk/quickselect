{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Control.Monad.LiftMany.Strict
  (liftM2
  ,liftM3
  ,liftM4
  ,liftM5
  ,(<$!>))
  where

#if MIN_VERSION_base(4,8,0)
import Control.Monad ((<$!>))
#else
infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
#endif

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f xs ys = do
    !x <- xs
    !y <- ys
#if MIN_VERSION_base(4,8,0)
    pure $! f x y
#else
    return $! f x y
#endif
{-# INLINE liftM2 #-}

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f xs ys zs = do
    !x <- xs
    !y <- ys
    !z <- zs
#if MIN_VERSION_base(4,8,0)
    pure $! f x y z
#else
    return $! f x y z
#endif
{-# INLINE liftM3 #-}

liftM4 :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4 f ws xs ys zs = do
    !w <- ws
    !x <- xs
    !y <- ys
    !z <- zs
#if MIN_VERSION_base(4,8,0)
    pure $! f w x y z
#else
    return $! f w x y z
#endif
{-# INLINE liftM4 #-}

liftM5 :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
liftM5 f vs ws xs ys zs = do
    !v <- vs
    !w <- ws
    !x <- xs
    !y <- ys
    !z <- zs
#if MIN_VERSION_base(4,8,0)
    pure $! f v w x y z
#else
    return $! f v w x y z
#endif
{-# INLINE liftM5 #-}
