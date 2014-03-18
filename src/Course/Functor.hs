{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P

class Functor f where
  -- Pronounced, eff-map.
  (<$>) :: (a -> b) -> f a -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
  -- data Id a = Id a
  -- (<$>) :: (a -> b) -> f a -> f b
  -- (<$>) :: (a -> b) -> Id a -> Id b
  (<$>)        f          (Id a) = Id (f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
-- (<$>) :: (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> List a -> List b
-- (<$>) :: f list -> map f list
  (<$>) f list = map f list

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
-- (<$>) :: (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> Optional a -> Optional b
  (<$>) _ Empty = Empty
  (<$>) f (Full a) = Full (f a)
    -- f :: a -> b
    -- a :: a
    -- f a : b
    -- ??? :: Optional b

    -- (<$>) f optional = mapOptional

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
-- (<$>) :: (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> ((->) t) a -> ((->) t) b
-- (<$>) :: (a -> b) -> (t -> a) -> (t -> b)
-- (<$>) :: (a -> b) -> (t -> a) -> t -> b
  (<$>)      f           g          t =
    -- f :: a -> b
    -- g :: t -> a
    -- t :: t
    -- g t :: a
    -- f ( g t) :: b
    -- undefined :: b
    f (g t)
    -- but this might be function composition
    -- fmap is the same as function composition

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ [1,2,3]
-- [7,7,7]
--
-- prop> x <$ [a,b,c] == [x,x,x]
--
-- prop> x <$ Full q == Full x
(<$) :: Functor f => a -> f b -> f a
(<$) = error "todo"

-- | Anonymous map producing unit value.
--
-- >>> void [1,2,3]
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap

instance Functor [] where
  (<$>) =
    P.fmap

instance Functor P.Maybe where
  (<$>) =
    P.fmap
