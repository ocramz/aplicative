{-# LANGUAGE TypeOperators #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
module Lib where

import Data.Foldable (toList)

class Functor f => Naperian f where
  type Log f
  lookup :: f a -> (Log f -> a )
  tabulate :: (Log f -> a ) -> f a
  positions :: f (Log f )

class (Applicative f, Naperian f, Traversable f) => Dimension f where
  size :: f a -> Int
  size = length . toList

class Shapely fs where
  hreplicate :: a -> Hyper fs a
  hsize :: Hyper fs a -> Int

first :: Shapely fs => Hyper fs a -> a
first (Scalar a ) = a
first (Prism x ) = head (toList (first x))

instance Shapely '[ ] where
  hreplicate a = Scalar a
  hsize = const 1
instance (Dimension f, Shapely fs ) => Shapely (f ': fs ) where
  hreplicate a = Prism (hreplicate (pure a))
  hsize (Prism x ) = size (first x ) * hsize x

data Hyper :: [* -> *] -> * -> * where -- final version
  Scalar :: a -> Hyper '[ ] a
  Prism :: (Dimension f ,Shapely fs ) => Hyper fs (f a) -> Hyper (f ': fs ) a

instance Functor (Hyper fs ) where
  fmap f (Scalar a ) = Scalar (f a)
  fmap f (Prism x ) = Prism (fmap (fmap f ) x )
