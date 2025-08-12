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

{- List -}
type List = []

newtype ListIter a = ListIter { lst :: List a }

instance Iterator ListIter where
    next =  liftM lst get >>= \(x:xs) -> -- unsafe!
            put (ListIter xs) >> return x

instance Iterable List where
    type Iter List = ListIter
    iterator = liftM ListIter get

instance Collection List where
    clear = put []
    contains x = liftM (elem x) get
    size = liftM length get
    add x = modify (x :)
    peek = liftM head get
    remove = get >>= \(x:xs) -> put xs >> return x
