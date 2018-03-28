{-# LANGUAGE AllowAmbiguousTypes  #-}
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

read :: MonadState s m => m s
read = State.get

write :: MonadState s m => s -> m ()
write = State.put

type RouteDsl a = ReaderT (DList PathPiece) (State (Routes a))

runRouteDsl :: RouteDsl r a -> [Route r]
runRouteDsl = DList.toList . flip execState mempty . flip runReaderT mempty

instance IsString PathPiece where
    fromString = Literal

type Routes a = DList (Route a)

data Route a = Leaf [PathPiece] (Endpoint a)
    deriving Show

data Endpoint a
    = Endpoint Verb a
    | MkSubsite a a a
    deriving (Show, Functor)

data PathPiece
    = Literal String
    | Capture Type
    deriving Show

(//) :: PathPiece -> RouteDsl r () -> RouteDsl r ()
pp // x = local (`DList.snoc` pp) x

infixr 5 //

data Type where
    Type :: Typeable t => Proxy t -> Type

instance Show Type where
    show (Type prxy) = show (typeRep prxy)

data Verb = Get | Put | Post | Delete
    deriving Show

renderVerb :: Verb -> String
renderVerb = map toUpper . show

get, put, post, delete :: r -> RouteDsl r ()
get = doVerb  Get
put = doVerb  Put
post = doVerb  Post
delete = doVerb  Delete

subsite :: String -> String -> String -> RouteDsl String ()
subsite name thing func = do
    let x = MkSubsite name thing func
    pcs <- asks DList.toList
    modify (`DList.snoc` Leaf pcs x)

doVerb :: Verb -> r -> RouteDsl r ()
doVerb verb r = do
    let x = Endpoint verb r
    pcs <- asks DList.toList
    modify (`DList.snoc` Leaf pcs x)

capture :: forall typ. Typeable typ => PathPiece
capture = Capture (Type (Proxy @typ))

type UserId = Int
type PostId = Int
type CommentId = Int

exampleDsl :: RouteDsl String ()
exampleDsl = do
    "users" // do
        get "IndexUserR"
        post "PostUserR"
        capture @UserId // do
            get "GetUserR"
            put "UpdateUserR"
            delete "DeleteUserR"
    "posts" // do
        get "IndexPostR"
        post "PostPostR"
        capture @PostId // do
            get "GetPostR"
            put "PutPostR"
            delete "DeletePostR"
            "comments" // do
                get "IndexCommentR"
                post "PostCommentR"
                capture @CommentId // do
                    get "GetCommentR"
                    put "PutCommentR"
