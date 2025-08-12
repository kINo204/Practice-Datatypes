{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}

import Prelude hiding (Monad(..), mapM, mapM_)
import MyMonad
import Data.Kind

class Iterator it where
    next :: Op (it a) a

class Iterable i where
    type Iter i :: Type -> Type
    iterator :: Op (i a) (Iter i a)

class
    Iterable c =>
    Collection c where

    clear       :: Op (c a) ()
    empty       :: Op (c a) Bool
    contains    :: Eq a => a -> Op (c a) Bool
    size        :: Op (c a) Int
    add         :: a -> Op (c a) ()
    delete      :: Eq a => a -> Op (c a) ()
    peek        :: Op (c a) a
    remove      :: Op (c a) a

    empty = liftM (0 ==) size
    delete x = delete' [] x where
               delete' reserved toDelete =
                    remove >>= \e ->
                    if toDelete == e then
                        mapM_ add reserved
                    else
                        delete' (e : reserved) x

class
    Collection l =>
    List l where

    at :: Int -> Op (l a) a
    removeAt :: Int -> Op (l a) a
    insertAt :: Int -> Op (l a) ()

class
    Collection s =>
    Stack s where

    push :: a -> Op (s a) ()
    pop  :: Op (s a) a

    push = add
    pop  = remove

class
    Collection q =>
    Queue q where

    enqueue :: a -> Op (q a) ()
    dequeue :: Op (q a) a

    enqueue = add
    dequeue = remove
