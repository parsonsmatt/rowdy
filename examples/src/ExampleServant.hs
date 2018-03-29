{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module ExampleServant where

import           Data.Typeable
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import           Rowdy.Servant

toServant "MyApi" $ do
    let unit = Type (Proxy @())
    "users" // do
        get unit
        -- need to add reqBody support
        post unit
        capture @Int // do
            get unit
            "posts" // do
                get unit
                post unit

handler :: Server MyApi
handler = getUsers :<|> postUsers :<|> getUser :<|> getPosts :<|> postPost

getUsers :: Handler ()
getUsers = pure ()

postUsers :: Handler ()
postUsers = pure ()

getUser :: Int -> Handler ()
getUser _ = pure ()

getPosts :: Int -> Handler ()
getPosts _ = pure ()

postPost :: Int -> Handler ()
postPost _ = pure ()

main :: IO ()
main = Warp.run 3000 (serve (Proxy @MyApi) handler)
