{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rowdy where

import           Control.Monad.Reader
import           Control.Monad.State   hiding (get, put)
import qualified Control.Monad.State   as State
import           Data.Char
import           Data.DList            (DList (..))
import qualified Data.DList            as DList
import           Data.Proxy
import           Data.String
import           Data.Typeable
import Data.Tree

read :: MonadState s m => m s
read = State.get

write :: MonadState s m => s -> m ()
write = State.put

type DForest a = DList (Tree a)

newtype RouteDsl capture endpoint a = RouteDsl
    { unRouteDsl :: ReaderT (DList capture) (State (DForest endpoint)) a
    } deriving
    ( Functor, Applicative, Monad
    , MonadReader (DList capture)
    , MonadState (DForest endpoint)
    )

runRouteDsl :: RouteDsl c e a -> Forest e
runRouteDsl =
    DList.toList . flip execState mempty . flip runReaderT mempty . unRouteDsl

(//)
    :: capture
    -> RouteDsl capture endpoint ()
    -> RouteDsl capture endpoint ()
pp // x = local (`DList.snoc` pp) x

terminal :: ([capture] -> endpoint) -> RouteDsl capture endpoint ()
terminal mkEndpoint = do
    captures <- asks DList.toList
    modify (`DList.snoc` Node (mkEndpoint captures) [])

infixr 5 //

-- data Type where
--     Type :: Typeable t => Proxy t -> Type
--
-- instance Show Type where
--     show (Type prxy) = show (typeRep prxy)
--
-- data Verb = Get | Put | Post | Delete
--     deriving Show
--
-- renderVerb :: Verb -> String
-- renderVerb = map toUpper . show
