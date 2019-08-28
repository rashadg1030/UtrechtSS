{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module MTA.State' where

import Control.Monad
import Control.Applicative


class Monad m => MonadState m s | m -> s where
    get :: m s
    get = modify id
    put :: s -> m ()
    put s = modify (const s) >> return ()
    modify :: (s -> s) -> m s
    modify f = do
        state <- get
        let newState = f state
        put newState
        return newState

data Counts = Counts { binds   :: Int
                     , returns :: Int
                     , gets    :: Int
                     , puts    :: Int
                     } deriving (Eq, Show)

instance Monoid Counts where
    mempty :: Counts
    mempty = Counts 0 0 0 0

instance Semigroup Counts where
   (Counts b1 r1 g1 p1) <> (Counts b2 r2 g2 p2) = Counts (b1 + b2) (r1 + r2) (g1 + g2) (p1 + p2)

oneBind, oneReturn, oneGet, onePut :: Counts
oneBind = Counts 1 0 0 0
oneReturn = Counts 0 1 0 0
oneGet = Counts 0 0 1 0
onePut = Counts 0 0 0 1

newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }

instance Functor (State' s) where
    fmap = liftA

instance Applicative (State' s) where
    pure = return
    (<*>) = ap

instance Monad (State' s) where
    return :: a -> State' s a
    return x = State' $ \(s, c) -> (x, s, c <> oneReturn)
    (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    (State' r) >>= f = State' $ \(st, c) ->
        let (a, newSt, count) = r (st, c)
            newCount = count <> oneBind
            (State' r') = f a
        in r' (newSt, newCount)

instance MonadState (State' s) s where
    get = State' (\(s, counts) -> (s, s, counts <> oneGet))
    put x = State' (\(_, counts) -> ((), x, counts <> onePut))
