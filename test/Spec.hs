import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Environment" $ do
    it "can run tests" $ do
      True `shouldBe` True
