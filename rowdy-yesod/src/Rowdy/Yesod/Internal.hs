
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | An internal module. Depend on this at your own risk -- breaking changes to
-- this module's interface will not be represented as a major version bump.
module Rowdy.Yesod.Internal where

import           Data.Char             (toUpper)
import           Data.Either           (isLeft, lefts, rights)
import           Data.Maybe            (isJust)
import           Data.String           (IsString (..))
import           Data.Typeable         (Proxy (..), Typeable, eqT, typeRep)
import           Yesod.Routes.TH.Types

import           Rowdy

-- | An endpoint in the Yesod model.
data Endpoint
    = MkResource Verb String
    -- ^ A resource identified by a 'Verb' and a 'String' name.
    | MkSubsite String String String
    -- ^ A subsite.
    deriving (Eq, Show)

-- | The type of things that can affect a path.
data PathPiece
    = Literal String
    -- ^ Static string literals.
    | Capture Type
    -- ^ Dynamic captures.
    | Attr String
    -- ^ Route attributes. Not technically part of the path, but applies to
    -- everything below it in the tree.
    deriving (Eq, Show)

instance IsString PathPiece where
    fromString = Literal

-- | A value containing a 'Proxy' of some Haskell type.
data Type where
    Type :: Typeable t => Proxy t -> Type

instance Show Type where
    show (Type prxy) = show (typeRep prxy)

instance Eq Type where
    Type (_ :: Proxy t0) == Type (_ :: Proxy t1) =
        isJust (eqT @t0 @t1)

-- | The HTTP verbs.
data Verb = Get | Put | Post | Delete
    deriving (Eq, Show)

-- | Render a verb as an uppercase string.
renderVerb :: Verb -> String
renderVerb = map toUpper . show

-- | Convert the Rowdy 'RouteTree' structure into one appropriate for the Yesod
-- routing functions.
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
