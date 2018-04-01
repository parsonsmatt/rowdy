{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module ExampleYesod where

import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Yesod.Core               (RenderRoute (..), Yesod, mkYesod,
                                           toWaiApp)

import           Rowdy.Yesod

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal

mkYesod "Minimal" $ toYesod $ do
    get "RootR"
    "users" // do
        resource "UserIndexR" [get, post]
        capture @Int // resource "UserR" [get, put]
    "admin" // "Admin" /: do
        get "PanelR" ! "admin" ! "cool"
        post "PanelR" ! "admin"
    "other-attr" // "safe" /! do
        get "SafeR"
        put "SafeR"

instance Yesod Minimal

getRootR :: Handler Text
getRootR = pure "Hello, world!"

getUserIndexR :: Handler ()
getUserIndexR = pure ()

postUserIndexR :: Handler ()
postUserIndexR = pure ()

getUserR :: Int -> Handler ()
getUserR _ = pure ()

putUserR :: Int -> Handler ()
putUserR _ = pure ()

getPanelR :: Handler ()
getPanelR = pure ()

postPanelR :: Handler ()
postPanelR = pure ()

getSafeR :: Handler ()
getSafeR = pure ()

putSafeR :: Handler ()
putSafeR = pure ()

main :: IO ()
main = run 3000 =<< toWaiApp Minimal
