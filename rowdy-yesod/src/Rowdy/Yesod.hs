{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Use your Rowdy route definitions with Yesod web applications.
module Rowdy.Yesod
    ( module Rowdy.Yesod
    , (//)
    , (/:)
    , Endpoint(..)
    , PathPiece(..)
    , Type(..)
    , Verb(..)
    ) where

import           Data.Foldable         (traverse_)
import           Data.Typeable         (Proxy (..), Typeable)
import           Yesod.Routes.TH.Types

import           Rowdy
import           Rowdy.Yesod.Internal

-- | Convert a 'RouteDsl' into a representation that Yesod can use.
--
-- @
-- mkYesod "App" $ toYesod $ do
--     get "RootR"
--     "users" // do
--        resource "UserIndex" [get, post]
--        -- etc...
-- @
--
-- GHC freaks out if you try to use a type defined in the same module as the
-- route. Ensure that all types you use in the route are defined in an imported
-- module.
--
-- @since 0.0.1.0
toYesod :: Dsl () -> [ResourceTree String]
toYesod = routeTreeToResourceTree . runRouteDsl

-- | We specialize the 'RouteDsl' type to this as a shorthand.
type Dsl = RouteDsl String PathPiece Endpoint

-- | We support the most common HTTP verbs. Each of 'get', 'put', 'post', and
-- 'delete' are given a @String@ that represents the resource they are acting
-- for. The generated route type uses that resource as a constructor. The
-- generated dispatcher expects to see functions with the lowercase verb as
-- a prefix to the resource name. As an example:
--
-- @
-- get "HelloR"
-- @
--
-- Will create a route type @HelloR@ and expect a handler @getHelloR@ to be
-- defined.
--
-- @since 0.0.1.0
get, put, post, delete :: String -> Dsl ()
get = doVerb Get
put = doVerb Put
post = doVerb Post
delete = doVerb Delete

-- | Create an endpoint with the given named resource and verb.
--
-- @since 0.0.1.0
doVerb :: Verb -> String -> Dsl ()
doVerb v s = terminal (MkResource v s)

-- | Create a subsite with the given @name@, @type@, and accessor function name
-- to get the underlying application.
--
-- @since 0.0.1.0
subsite :: String -> String -> String -> Dsl ()
subsite name thing func =
    terminal (MkSubsite name thing func)

-- | Capture a dynamic path piece and parse it as the provided type. This
-- function is intended to be used with the @TypeApplications@ language
-- extension.
--
-- @
-- "users" // do
--     resource "UserIndexR" [get, post]
--     capture @UserId $ do
--         resource "UserR" [get, put, delete]
--         "posts" // do
--             resource "PostR" [get, post]
-- @
--
-- @since 0.0.1.0
capture :: forall typ. Typeable typ => PathPiece
capture =
    captureP (Proxy @typ)

-- | A version of 'capture' that accepts an explicit 'Proxy' argument. Use this
-- if you don't like the @TypeApplications@ syntax, or have a proxy on hand
-- already.
--
-- @since 0.0.1.0
captureP :: forall typ. Typeable typ => Proxy typ -> PathPiece
captureP = Capture . Type

-- | Define a number of handlers for the named resource. The following code
-- block:
--
-- @
-- do 'get' "HelloR"
--    'put' "HelloR"
--    'post' "HelloR"
--    'delete' "HelloR"
-- @
--
-- is equivalent to this shorter form:
--
-- @
-- 'resource' "HelloR" ['get', 'put', 'post', 'delete']
-- @
--
-- @since 0.0.1.0
resource :: String -> [String -> Dsl ()] -> Dsl ()
resource = traverse_ . flip id

-- | Attach a route attribute to every element in the given DSL.
--
-- @since 0.0.1.0
attr :: String -> Dsl () -> Dsl ()
attr = pathComponent . Attr

-- | An infix operator alias for 'attr'.
--
-- @
-- "admin" // "admin" /! do
--      'resource' "AdminR" ['get', 'put', 'post']
-- @
--
-- @since 0.0.1.0
(/!) :: String -> Dsl () -> Dsl ()
(/!) = attr

infixr 8 /!

-- | Provide an inline attribute to the given route.
--
-- @
-- get "HelloR" ! "friendly"
-- @
(!) :: Dsl () -> String -> Dsl ()
(!) = flip attr

infixl 8 !
