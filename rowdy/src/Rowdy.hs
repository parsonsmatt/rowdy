{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | Rowdy is a DSL for defining web routes. The DSL is only a nice syntax for
-- expressing the tree-like structure of routes -- to actually interpret the
-- route into something useful, you'll need another package.
--
-- @rowdy-yesod@ provides a function that converts this representation into the
-- Yesod route format, allowing you to drop the quasiquotater and use a plain
-- Haskell DSL.
--
-- @rowdy-servant@ provides a function that converts the DSL into Servant's type
-- level DSL for defining routes. This allows you to work with a value-level
-- DSL, taking full advantage of Haskell's value level programming, and then
-- promote the DSL to the type level using Template Haskell.
module Rowdy where

import           Control.Monad.Writer
import           Data.DList           (DList (..))
import qualified Data.DList           as DList

-- | A 'RouteDsl' is a type useful for constructing web routes. At it's heart,
-- it is a DSL for constructing a 'RouteTree', and is totally optional.
--
-- Routes are defined by how they handle @nest@ing, what sorts of values are
-- used to represent @capture@s, and what values are used to represent
-- endpoints.
--
-- @since 0.0.1.0
newtype RouteDsl nest capture terminal a = RouteDsl
    { unRouteDsl :: Writer (DForest nest capture terminal) a
    } deriving
    ( Functor, Applicative, Monad
    , MonadWriter (DForest nest capture terminal)
    )

-- | Run the given 'RouteDsl' and convert it into the 'Forest' of routes. If you
-- are defining an interpreter for a web framework, you will want to call this
-- on the 'RouteDsl' value.
--
-- @since 0.0.1.0
runRouteDsl :: RouteDsl n c e a -> Forest n c e
runRouteDsl =
    DList.toList . execWriter . unRouteDsl

-- | Run the given 'RouteDsl' and convert it into a 'DList' of routes. This is
-- useful when implementing combinators.
--
-- @since 0.0.1.0
runRouteDsl' :: RouteDsl n c e a -> DForest n c e
runRouteDsl' = execWriter . unRouteDsl

-- | Introduce a @capture@ into all of the routes defined in the second
-- argument. This function does not introduce nesting, so multiple distinct
-- routes will be created.
--
-- As an example:
--
-- @
-- example :: RouteDsl nest String String ()
-- example =
--     'pathComponent' "hello" $ do
--         'terminal' "first route"
--         'terminal' "second route"
-- @
--
-- Calling @'runRouteDsl' example@ will give a data structure like:
--
-- @
-- [ 'PathComponent' "hello" ('Leaf' "first route")
-- , 'PathComponent' "hello" ('Leaf' "second route")
-- ]
-- @
--
-- @since 0.0.1.0
pathComponent
    :: capture
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
pathComponent pp =
    tell . fmap (PathComponent pp) . runRouteDsl'

-- | An infix operator for 'pathComponent'.
--
-- @since 0.0.1.0
(//)
    :: capture
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
(//) = pathComponent

infixr 5 //

-- | Introduce a nesting point in the route DSL. While the 'pathComponent'
-- function adds the @capture@ to each route defined in the second argument,
-- this one preserves the tree-like structure of the declaration.
--
-- @
-- example :: 'RouteDsl' String String String ()
-- example =
--     'pathComponent' "thing" $ 'nest' "hello" $ do
--          terminal "first"
--          terminal "second"
-- @
--
-- Calling @'runRouteDsl' example@ would give a data structure like:
--
-- @
-- [ 'PathComponent' "thing" ('Nest'
--     [ Leaf "first"
--     , Leaf "second"
--     ]
--   )
-- ]
-- @
--
-- In constrast, if 'nest' were not called, you would see the 'PathComponent'
-- repeated and distributed to both endpoints.
--
-- @since 0.0.1.0
nest
    :: nest
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
nest str = tell . pure . Nest str . runRouteDsl

-- | An infix operator alias for 'nest'.
--
-- @since 0.0.1.0
(/:)
    :: nest
    -> RouteDsl nest capture endpoint ()
    -> RouteDsl nest capture endpoint ()
(/:) = nest

infixr 7 /:

-- | Record the given @endpoint@ as a route.
--
-- @since 0.0.1.0
terminal :: endpoint -> RouteDsl nest capture endpoint ()
terminal = tell . pure . Leaf

-- | Convert a 'RouteTree' into a flattened list of routes. Each @terminal@ is
-- paired with the list of @capture@s that preceeded it.
--
-- @since 0.0.1.0
unnest :: RouteTree nest capture terminal -> [([capture], terminal)]
unnest = go mempty
  where
    go caps (Leaf term) =
        [(DList.toList caps, term)]
    go caps (PathComponent cap next) =
        go (DList.snoc caps cap) next
    go caps (Nest _ xs) =
        concatMap (go caps) xs


-- | For efficiency's sake, we encode the route DSL as a 'DList' while defining
-- them, and (for convenience's sake) we present them as an ordinary list when
-- you run it. To prevent type complexity, we parameterize the forest on how
-- we're working with it.
--
-- @since 0.0.1.0
type ForestOf f n capture terminal = f (RouteTree n capture terminal)

-- | A difference list ('DList') of 'RouteTree' values.
--
-- @since 0.0.1.0
type DForest n c t = ForestOf DList n c t

-- | A list of 'RouteTree' values.
--
-- @since 0.0.1.0
type Forest n c t = ForestOf [] n c t

-- | The core data type that is produced by the 'RouteDsl'. If you'd prefer
-- a non-monadic interface to creating these, you're welcome to use the
-- constructors directly.
--
-- The DSL defined as @example@ below has the route representation given by
-- @desugared@:
--
-- @
-- example :: 'Forest' String String String
-- example = 'runRouteDsl' $ do
--     "hello" // do
--         'terminal' "world"
--         'terminal' "friend"
--         "nest" /: do
--             'terminal' "nope"
--             'terminal' "yes"
--
-- desugared :: 'Forest' String String String
-- desugared =
--     [ 'PathComponent' "hello" ('Leaf' "world")
--     , 'PathComponent' "hello" ('Leaf' "friend")
--     , 'PathComponent' "hello" ('Nest' "nest"
--         [ 'Leaf' "nope"
--         , 'Leaf' "yes"
--         ]
--       )
--     ]
-- @
--
-- @since 0.0.1.0
data RouteTree nest capture terminal
    = Leaf terminal
    | PathComponent capture (RouteTree nest capture terminal)
    | Nest nest [RouteTree nest capture terminal]
    deriving (Eq, Show, Functor, Foldable)
