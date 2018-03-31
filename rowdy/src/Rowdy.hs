{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Rowdy where

import Control.Monad.Writer
import           Data.DList           (DList (..))
import qualified Data.DList           as DList

type ForestOf f capture terminal = f (RouteTree capture terminal)
type DForest c t = ForestOf DList c t
type Forest c t = ForestOf [] c t

data RouteTree capture terminal
    = Leaf terminal
    | PathComponent capture (RouteTree capture terminal)
    | Nest [RouteTree capture terminal]
    deriving (Eq, Show, Functor, Foldable)

newtype RouteDsl capture terminal a = RouteDsl
    { unRouteDsl :: Writer (DForest capture terminal) a
    } deriving
    ( Functor, Applicative, Monad
    , MonadWriter (DForest capture terminal)
    )

runRouteDsl :: RouteDsl c e a -> Forest c e
runRouteDsl =
    DList.toList . execWriter . unRouteDsl

(//)
    :: capture
    -> RouteDsl capture endpoint ()
    -> RouteDsl capture endpoint ()
pp // x =
    case runRouteDsl x of
        [] -> error "what is wrong with you"
        [a] -> tell (pure (PathComponent pp a))
        xs -> tell (pure (PathComponent pp (Nest xs)))

terminal :: endpoint -> RouteDsl capture endpoint ()
terminal = tell . pure . Leaf

infixr 5 //
