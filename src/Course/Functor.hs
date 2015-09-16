{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P

{- Some notes on type-classes, from T. Morris 2015-09-16T0922:

  Eq a                          -- :info Eq

    (==) :: a -> a ->  Bool
    (/=) :: a -> a -> Bool

  Ord a

    compare :: a -> a -> GHC.Types.Ordering
    (<) :: a -> a -> Bool, &c.

A type-class is defined by Class, then Instance makes a data-type
belong to a type-class, or this can be done using "deriving" in simple
cases.  Examples of type-classes are Eq, Ord, & Show.

Designing type-classes is difficult.  For example, the types in Ord
should be strict subset of Eq; the operators on Eq should be a subset
of those on Ord.  The former inclusion is given by the _constraint_
"class Eq a => Ord a where"; the latter is implied by the former.

-}

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

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
  (<$>) ::
    (a -> b)
    -> Id a
    -> Id b
  f <$> Id x = Id (f x)         -- gdmcbain 2015-09-16T1004

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) = map                   -- gdmcbain 2015-09-15T1833

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) = mapOptional           -- gdmcbain 2015-09-15T1836

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  (<$>) = (.)                   -- gdmcbain 2015-09-15T1902

-- http://bartoszmilewski.com/2015/01/20/functors (gdmcbain)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ [1,2,3]
-- [7,7,7]
--
-- prop> x <$ [a,b,c] == [x,x,x]
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
--(<$) c = (<$>) (const c)      -- gdmcbain 2015-09-15T1841
(<$) = (<$>) . const            -- 2015-09-16T1103

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
void = (<$) ()                  -- gdmcbain 2015-09-15T1846


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
