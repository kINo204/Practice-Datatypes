module MyMonad (
    Monad(..),
    Op(..), (>>>), nul, put, get, modify,
    when, unless, mapM, mapM_, replicateM, replicateM_, liftM,
) where

import Prelude hiding (Monad(..), mapM, mapM_)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    x >> y = x >>= const y


instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    Just x >>= f = f x


{- State monad -}
-- The name "State" is inaccurate, using "Op"(operation) instead.
newtype Op s a = Op { run :: s -> (s, Maybe a) }

(>>>) :: s -> Op s a -> (s, Maybe a)
s >>> op = run op s
infixr 0 >>>

instance Monad (Op s) where
    return x = Op run where run s = (s, Just x)
    Op r0 >>= f = Op r
            where r s0 = let (s1, r1) = r0 s0
                         in case r1 of
                            Nothing -> (s1, Nothing) -- stays at last state
                            Just x1 -> s1 >>> f x1

nul :: Op s a
nul = Op (\s -> (s, Nothing))

put :: s -> Op s ()
put s = Op (const (s, Just ()))

get :: Op s s
get = Op (\s -> (s, Just s))

modify :: (s -> s) -> Op s ()
modify f = Op (\s -> (f s, Just ()))


{- Monadic control structures -}
when :: Monad m => Bool -> m () -> m ()
when cond = if cond
            then id
            else const $ return ()

unless :: Monad m => Bool -> m () -> m ()
unless cond = when $ not cond

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (p:ps) = f p         >>= \x ->
                mapM f ps   >>= \xs ->
                return (x:xs)

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ _ [] = return ()
mapM_ f (p:ps) = f p        >>
                mapM_ f ps  >>
                return ()

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n op = op >>= \x ->
                 replicateM (n - 1) op >>= \xs ->
                 return (x:xs)

replicateM_ :: (Monad m) => Int -> m a -> m ()
replicateM_ 0 _ = return ()
replicateM_ n op = op >>
                 replicateM_ (n - 1) op >>
                 return ()

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f op = op >>= \a -> return (f a)
