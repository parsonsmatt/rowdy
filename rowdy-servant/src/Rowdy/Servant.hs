{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Rowdy.Servant
    ( module Rowdy.Servant
    , get, put, post, capture, (//), Rowdy.Type(..)
    ) where

import           Data.Typeable
import           Language.Haskell.TH      as TH
import           Servant.API              as Servant
import           Servant.API.Capture              as Servant
import           Servant.API.ContentTypes as Servant

import           Rowdy

toServant :: String -> RouteDsl Rowdy.Type () -> Q [Dec]
toServant apiName = renderRoutes . runRouteDsl
  where
    renderRoutes =
        fmap pure
            . tySynD (mkName apiName) []
            . pure
            . foldr1 (\x acc -> ConT ''(:<|>) `AppT` x `AppT` acc)
            . map routeToType

    routeToType (Leaf pcs (Endpoint verb (Rowdy.Type prxy))) =
        let pcs' =
                map pieceToType pcs
            end =
                verbToVerb verb
                    `AppT` json
                    `AppT` (ConT . mkName . show . typeRep) prxy
         in
            foldr (\x acc -> ConT ''(:>) `AppT` x `AppT` acc) end pcs'
    routeToType _ =
        error "mkSubsite is only for yesod"

    json =
        PromotedConsT
            `AppT` ConT ''JSON
            `AppT` PromotedNilT

    verbToVerb = ConT . \case
        Get -> ''Servant.Get
        Put ->  ''Servant.Put
        Post -> ''Servant.Post
        Put ->  ''Servant.Put

    pieceToType (Literal str) =
        LitT (StrTyLit str)
    pieceToType (Capture (Type prxy)) =
        ConT ''Capture
            `AppT` (LitT . StrTyLit . show $ typeRep prxy)
            `AppT` (ConT . mkName . show $ typeRep prxy)
