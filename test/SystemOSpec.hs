module SystemOSpec(spec) where
import Test.Hspec (Spec, describe, it, shouldBe)
import SystemO (someFunc)

spec :: Spec
spec = do
    describe "someFunc" $ do
        it "is 0" $ do
            someFunc `shouldBe` 0
