{-# language TypeApplications, OverloadedStrings, StandaloneDeriving, FlexibleInstances #-}

module Rowdy.YesodSpec where

import Data.Proxy
import Rowdy
import Rowdy.Yesod
import Yesod.Routes.TH.Types
import Test.Hspec

deriving instance Eq (ResourceTree String)
deriving instance Eq (Resource String)
deriving instance Eq (Piece String)
deriving instance Eq (Dispatch String)

router = do
    get "RootR"
    "weight" // do
        get "WeightsR"
        "record" // do
            resource "RecordWeightR" [get, post]
        capture @Int // do
            delete "WeightR"

    "auth" // do
        resource "LoginR" [get, post]
        post "LogoutR"


spec :: Spec
spec = do
    describe "routes" $ do
        it "works" $ do
            runRouteDsl router
                `shouldBe`
                    [ Leaf (MkResource Get "RootR")
                    , PathComponent (Literal "weight")
                        $ Leaf (MkResource Get "WeightsR")
                    , PathComponent (Literal "weight")
                        $ PathComponent (Literal "record")
                        $ Leaf (MkResource Get "RecordWeightR")
                    , PathComponent (Literal "weight")
                        $ PathComponent (Literal "record")
                        $ Leaf (MkResource Post "RecordWeightR")
                    , PathComponent (Literal "weight")
                        $ PathComponent (Capture (Type (Proxy @Int)))
                        $ Leaf (MkResource Delete "WeightR")
                    , PathComponent (Literal "auth")
                        $ Leaf (MkResource Get "LoginR")
                    , PathComponent (Literal "auth")
                        $ Leaf (MkResource Post "LoginR")
                    , PathComponent (Literal "auth")
                        $ Leaf (MkResource Post "LogoutR")
                    ]

        it "toYesod works" $ do
            toYesod router
                `shouldBe`
                    [ ResourceLeaf (Resource "RootR" [] (Methods Nothing ["GET"]) [] True)
                    , ResourceLeaf (Resource "WeightsR" [Static "weight"] (Methods Nothing ["GET"]) [] True)
                    , ResourceLeaf (Resource "RecordWeightR" [Static "weight", Static "record"] (Methods Nothing ["GET", "POST"]) [] True)
                    , ResourceLeaf (Resource "WeightR" [Static "weight", Dynamic "Int"] (Methods Nothing ["DELETE"]) [] True)
                    , ResourceLeaf (Resource "LoginR" [Static "auth"] (Methods Nothing ["GET", "POST"]) [] True)
                    , ResourceLeaf (Resource "LogoutR" [Static "auth"] (Methods Nothing ["POST"]) [] True)
                    ]
