{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Rowdy.Yesod
    ( module Rowdy.Yesod
    , (//)
    , (/:)
    ) where

import           Data.Char
import           Data.Either
import           Data.Foldable
import           Data.Maybe            (isJust)
import           Data.String
import           Data.Typeable
import           Yesod.Routes.TH.Types

import           Rowdy

type Dsl = RouteDsl String PathPiece Endpoint

toYesod :: Dsl () -> [ResourceTree String]
toYesod = routeTreeToResourceTree . toList . runRouteDsl

routeTreeToResourceTree :: [RouteTree String PathPiece Endpoint] -> [ResourceTree String]
routeTreeToResourceTree =
    foldr (go []) []
  where
    go
        :: [Either String (Piece String)]
        -> RouteTree String PathPiece Endpoint
        -> [ResourceTree String]
        -> [ResourceTree String]
    go pcs (Nest str xs) acc =
        ResourceParent str True pieces (foldr (go attrs) [] xs) : acc
      where
        pieces = rights (reverse pcs)
        attrs = filter isLeft pcs
    go pcs (PathComponent pp rest) acc =
         go (convPiece pp : pcs) rest acc
    go pcs (Leaf term) (ResourceLeaf Resource {..} : acc)
        | listEq eqPieceStr (rights (reverse pcs)) resourcePieces
        , Methods multi methods <- resourceDispatch
        =
        flip (:) acc . ResourceLeaf $
            case term of
                MkResource v str ->
                    Resource
                        { resourceName = str
                        , resourcePieces = rights (reverse pcs)
                        , resourceDispatch =
                            Methods
                                { methodsMulti = multi
                                , methodsMethods = renderVerb v : methods
                                }
                        , resourceAttrs =
                            lefts pcs
                        , resourceCheck =
                            True
                        }
                MkSubsite str typ func ->
                    Resource
                        { resourceName = str
                        , resourcePieces = reverse (rights pcs)
                        , resourceDispatch =
                            Subsite
                                { subsiteType = typ
                                , subsiteFunc = func
                                }
                        , resourceAttrs =
                            lefts pcs
                        , resourceCheck =
                            True
                        }
    go pcs (Leaf term) acc =
        flip (:) acc . ResourceLeaf $
            case term of
                MkResource v str ->
                    Resource
                        { resourceName = str
                        , resourcePieces = reverse (rights pcs)
                        , resourceDispatch =
                            Methods
                                { methodsMulti = Nothing
                                , methodsMethods = [renderVerb v]
                                }
                        , resourceAttrs =
                            lefts pcs
                        , resourceCheck =
                            True
                        }
                MkSubsite str typ func ->
                    Resource
                        { resourceName = str
                        , resourcePieces = reverse (rights pcs)
                        , resourceDispatch =
                            Subsite
                                { subsiteType = typ
                                , subsiteFunc = func
                                }
                        , resourceAttrs =
                            lefts pcs
                        , resourceCheck =
                            True
                        }

convPiece :: PathPiece -> Either String (Piece String)
convPiece = \case
    Literal str -> Right (Static str)
    Capture (Type prxy) -> Right (Dynamic (show (typeRep prxy)))
    Attr attr -> Left attr

listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq f (x:xs) (y:ys) = f x y && listEq f xs ys
listEq _ [] []         = True
listEq _ _ _           = False

eqPieceStr :: Piece String -> Piece String -> Bool
eqPieceStr (Static s2) (Static s1)   = s1 == s2
eqPieceStr (Dynamic d0) (Dynamic d1) = d0 == d1
eqPieceStr _ _                       = False

data Endpoint
    = MkResource Verb String
    | MkSubsite String String String
    deriving (Eq, Show)

data PathPiece
    = Literal String
    | Capture Type
    | Attr String
    deriving (Eq, Show)

instance IsString PathPiece where
    fromString = Literal

data Type where
    Type :: Typeable t => Proxy t -> Type

instance Show Type where
    show (Type prxy) = show (typeRep prxy)

instance Eq Type where
    Type (_ :: Proxy t0) == Type (_ :: Proxy t1) =
        isJust (eqT @t0 @t1)

data Verb = Get | Put | Post | Delete
    deriving (Eq, Show)

renderVerb :: Verb -> String
renderVerb = map toUpper . show

get, put, post, delete :: String -> Dsl ()
get = doVerb  Get
put = doVerb  Put
post = doVerb  Post
delete = doVerb  Delete

subsite :: String -> String -> String -> Dsl ()
subsite name thing func =
    terminal (MkSubsite name thing func)

doVerb :: Verb -> String -> Dsl ()
doVerb verb r =
    terminal (MkResource verb r)

capture :: forall typ. Typeable typ => PathPiece
capture =
    Capture (Type (Proxy @typ))

resource :: String -> [String -> Dsl ()] -> Dsl ()
resource = traverse_ . flip id

attr :: String -> Dsl () -> Dsl ()
attr = pathComponent . Attr

(/!) :: String -> Dsl () -> Dsl ()
(/!) = attr

infixr 8 /!

(!) :: Dsl () -> String -> Dsl ()
(!) = flip attr

infixl 8 !

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst []     = []
