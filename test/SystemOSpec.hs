{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
module SystemOSpec(spec) where
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.Hspec.QuickCheck ( prop )
import SystemO (emptyApi, isSubApiOf)
import Data.OpenApi as O ( OpenApi )
import Data.Aeson as J ( decode )
import SystemO.Arbitraries (unNoop, unFill, unReducer)

spec :: Spec
spec = do
    describe "SystemO" $ do
        describe "subtype relationship" $ do
            describe "for empty api" $ do
                it "is reflexive" $ do
                    let actual = emptyApi `isSubApiOf` emptyApi
                        expected = True
                    actual `shouldBe` expected
            
                prop "is always a subtype of a noop modifed form " $
                    \op -> isSubApiOf emptyApi $ unNoop op emptyApi
            -- describe "a filled api" $ do
            --     prop "is a never a subtype of a reduced form of itself" $
            --         \api reducer -> not (isSubApiOf (unFill api) (unReducer reducer $ unFill api))

        describe "example" $ do
            it "parses example" $ do
                let
                    expected = J.decode @O.OpenApi "{\"openapi\":\"3.0.0\",\"info\":{\"title\":\"\",\"version\":\"\"},\"components\":{}}"
                    actual = Just emptyApi
                actual `shouldBe` expected


