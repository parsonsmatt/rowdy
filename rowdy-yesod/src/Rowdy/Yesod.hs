module Rowdy.Yesod
    ( module Rowdy.Yesod
    , get, put, post, capture, (//)
    ) where

import           Data.Typeable
import           Yesod.Routes.TH.Types

import           Rowdy

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


