{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Rowdy.Servant
    ( module Rowdy.Servant
    , (//), SomeType(..)
    ) where

import           Data.String
import           Data.Typeable
import           Language.Haskell.TH as TH
import           Servant.API         as Servant

import           Rowdy

type Dsl = RouteDsl () PathPiece (Route SomeType)

toServant :: String -> Dsl () -> Q [Dec]
toServant apiName = renderRoutes . runRouteDsl
  where
    renderRoutes =
        fmap pure
            . tySynD (mkName apiName) []
            . pure
            . foldr1 (\x acc -> ConT ''(:<|>) `AppT` x `AppT` acc)
            . map (uncurry routeToType)
            . concatMap unnest

    routeToType pcs (Route (MkResource verb (SomeType prxy))) =
        let pcs' = map pieceToType pcs
            end = verbToVerb verb
                `AppT` json
                `AppT` (ConT . mkName . show . typeRep) prxy
         in foldr (\x acc -> ConT ''(:>) `AppT` x `AppT` acc) end pcs'

    json =
        PromotedConsT
            `AppT` ConT ''JSON
            `AppT` PromotedNilT

    verbToVerb = ConT . \case
        Get -> ''Servant.Get
        Put ->  ''Servant.Put
        Post -> ''Servant.Post
        Delete ->  ''Servant.Delete

    pieceToType (Literal str) =
        LitT (StrTyLit str)
    pieceToType (Capture (SomeType prxy)) =
        ConT ''Capture
            `AppT` (LitT . StrTyLit . show $ typeRep prxy)
            `AppT` (ConT . mkName . show $ typeRep prxy)

newtype Route a = Route (Endpoint a)

data Endpoint a
    = MkResource RVerb a
    deriving (Show, Functor)

data RVerb = Get | Put | Post | Delete
    deriving (Eq, Show)

data PathPiece
    = Literal String
    | Capture SomeType
    deriving Show

instance IsString PathPiece where
    fromString = Literal

data SomeType where
    SomeType :: Typeable t => Proxy t -> SomeType

instance Show SomeType where
    show (SomeType prxy) = show (typeRep prxy)

get, put, post, delete :: SomeType -> Dsl ()
get = doVerb  Get
put = doVerb  Put
post = doVerb Post
delete = doVerb Delete

doVerb :: RVerb -> SomeType -> Dsl ()
doVerb verb r = terminal (Route (MkResource verb r))

capture :: forall typ. Typeable typ => PathPiece
capture = Capture (SomeType (Proxy @typ))
