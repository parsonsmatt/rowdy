{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
-- {-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Rowdy.Class where

import Data.String
-- import Control.Monad.State

data HttpVerb = Get | Put | Post | Delete
    deriving Show

data SubsiteDef = SubsiteDef

class Monad m => Endpoint m where
    record :: m ()

class Endpoint m => Verb m where
    verb :: HttpVerb -> m ()

class Endpoint m => Subsite m where
    forward :: SubsiteDef -> m ()

class PathPiece a m where
    enlist :: a -> m ()

class HasPath x m where
    addPath :: x -> m ()
    getPath :: m [x]

instance HasPath String m => PathPiece String m where
    enlist = addPath

(//) :: Monad m => PathPiece a m => Endpoint m => a -> m () -> m ()
a // m = do
    enlist a
    m

infixr 7 //

get, put, post, delete :: Verb m => m ()
get = verb Get
put = verb Put
post = verb Post
delete = verb Delete

example :: (HasPath String m, Verb m, Subsite m) => m ()
example = do
    "users" // "get" // "wrecked" // post
    "tagless" // do
        "final" // do
            get
    "fuck" // forward SubsiteDef
