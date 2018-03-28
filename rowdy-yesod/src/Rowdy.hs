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
import           Yesod.Routes.TH.Types

read :: MonadState s m => m s
read = State.get

write :: MonadState s m => s -> m ()
write = State.put

type RouteDsl a = ReaderT (DList PathPiece) (State (Routes a))

runRouteDsl :: RouteDsl r a -> [Route r]
runRouteDsl = DList.toList . flip execState mempty . flip runReaderT mempty

toYesod :: RouteDsl String () -> [ResourceTree String]
toYesod = foldr go [] . runRouteDsl
  where
    go l [] = [convert l]
    go l@(Leaf pieces endpoint) (ResourceLeaf res : rest)
        | listEq eqPieceStr pcs (resourcePieces res)
        , Methods multi methods <- resourceDispatch res
        =
            case endpoint of
                Endpoint verb name ->
                    let res' = res
                            { resourceDispatch =
                                Methods multi (verb' : methods)
                            }
                        verb' = renderVerb verb
                     in if verb' `elem` methods
                            then ResourceLeaf res : rest -- a duplicate!
                            else ResourceLeaf res' : rest
                MkSubsite name typ func ->
                    error "subsite overlap"
        | otherwise =
            convert l : ResourceLeaf res : rest
      where
        pcs = map convPiece pieces

    convert (Leaf pcs endpoint) =
        case endpoint of
            Endpoint verb name ->
                ResourceLeaf Resource
                    { resourceName =
                        name
                    , resourcePieces =
                        map convPiece pcs
                    , resourceDispatch = -- need a way to add a subsite option?
                        Methods Nothing [renderVerb verb]
                    , resourceAttrs =
                        [] -- need to add attributes to the dsl
                    , resourceCheck =
                        True -- need to add whether or not to check for overlap here
                    }
            MkSubsite name thing func ->
                ResourceLeaf Resource
                    { resourceName =
                        name
                    , resourcePieces =
                        map convPiece pcs
                    , resourceDispatch = -- need a way to add a subsite option?
                        Subsite thing func
                    , resourceAttrs =
                        [] -- need to add attributes to the dsl
                    , resourceCheck =
                        True -- need to add whether or not to check for overlap here
                    }



    convPiece (Literal str) =
        Static str
    convPiece (Capture (Type prxy)) =
        Dynamic (show (typeRep prxy))

listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq f (x:xs) (y:ys) = f x y && listEq f xs ys
listEq f [] []         = True
listEq _ _ _           = False

eqPieceStr :: Piece String -> Piece String -> Bool
eqPieceStr (Static s2) (Static s1)   = s1 == s2
eqPieceStr (Dynamic d0) (Dynamic d1) = d0 == d1
eqPieceStr _ _                       = False

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
