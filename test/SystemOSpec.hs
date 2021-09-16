
module SystemOSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldBe)
import SystemO (someFunc, exampleApi)
import Data.Text.Lazy as T

spec :: Spec
spec = do
    describe "SystemO" $ do
        describe "someFunc" $ do
            it "is 0" $ do
                someFunc `shouldBe` 0
        describe "example" $ do
            it "parses example" $ do
                actual <- exampleApi 
                actual `shouldBe` expected
                where 
                    expected = Just $ T.fromStrict "{\"openapi\":\"3.0.0\",\"info\":{\"title\":\"\",\"version\":\"\"},\"components\":{}}"


