{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Rowdy.Yesod
    ( module Rowdy.Yesod
    , (//)
    ) where

import           Data.Typeable
import           Yesod.Routes.TH.Types
import Data.String
import Data.Foldable
import Data.Char

import           Rowdy

type Dsl = RouteDsl PathPiece (Route String)

toYesod :: Dsl () -> [ResourceTree String]
toYesod = foldr go [] . concatMap toList . toList . runRouteDsl
  where
    go l [] = [convert l]
    go l@(Route pieces endpoint) (ResourceLeaf res : rest)
        | listEq eqPieceStr pcs (resourcePieces res)
        , Methods multi methods <- resourceDispatch res
        =
            case endpoint of
                MkResource verb name ->
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

    convert (Route pcs endpoint) =
        case endpoint of
            MkResource verb name ->
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

data Route a = Route [PathPiece] (Endpoint a)

data Endpoint a
    = MkResource Verb a
    | MkSubsite a a a
    deriving (Show, Functor)

data PathPiece
    = Literal String
    | Capture Type
    deriving Show

instance IsString PathPiece where
    fromString = Literal

data Type where
    Type :: Typeable t => Proxy t -> Type

instance Show Type where
    show (Type prxy) = show (typeRep prxy)

data Verb = Get | Put | Post | Delete
    deriving Show

renderVerb :: Verb -> String
renderVerb = map toUpper . show

get, put, post, delete :: String -> Dsl ()
get = doVerb  Get
put = doVerb  Put
post = doVerb  Post
delete = doVerb  Delete

subsite :: String -> String -> String -> Dsl ()
subsite name thing func =
    terminal (\pcs -> Route pcs (MkSubsite name thing func))

doVerb :: Verb -> String -> Dsl ()
doVerb verb r = terminal (\pcs -> Route pcs (MkResource verb r))

capture :: forall typ. Typeable typ => PathPiece
capture = Capture (Type (Proxy @typ))
