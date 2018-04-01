{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Rowdy where

import           Control.Monad.Writer
import           Data.DList           (DList (..))
import qualified Data.DList           as DList

type ForestOf f n capture terminal = f (RouteTree n capture terminal)
type DForest n c t = ForestOf DList n c t
type Forest n c t = ForestOf [] n c t

data RouteTree nest capture terminal
    = Leaf terminal
    | PathComponent capture (RouteTree nest capture terminal)
    | Nest nest [RouteTree nest capture terminal]
    deriving (Eq, Show, Functor, Foldable)

newtype RouteDsl nest capture terminal a = RouteDsl
    { unRouteDsl :: Writer (DForest nest capture terminal) a
    } deriving
    ( Functor, Applicative, Monad
    , MonadWriter (DForest nest capture terminal)
    )

runRouteDsl :: RouteDsl n c e a -> Forest n c e
runRouteDsl =
    DList.toList . execWriter . unRouteDsl

pathComponent
    :: capture
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
pathComponent pp =
    tell . DList.fromList . map (PathComponent pp) . runRouteDsl

(//)
    :: capture
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
(//) = pathComponent

infixr 5 //

nest
    :: nest
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
nest str = tell . pure . Nest str . runRouteDsl

(/:)
    :: nest
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
(/:) = nest

infixr 7 /:

terminal :: endpoint -> RouteDsl nest capture endpoint ()
terminal = tell . pure . Leaf

unnest :: RouteTree nest capture terminal -> [([capture], terminal)]
unnest = go mempty
  where
    go caps (Leaf term) =
        [(DList.toList caps, term)]
    go caps (PathComponent cap next) =
        go (DList.snoc caps cap) next
    go caps (Nest _ xs) =
        concatMap (go caps) xs
