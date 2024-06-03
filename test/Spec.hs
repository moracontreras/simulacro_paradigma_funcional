import Test.Hspec
import Parcial

main :: IO ()
main = hspec $ do
  describe "doble multiplica a un número por 2" $ do
    it "el doble de 2 es 4" $ do
      doble 2 `shouldBe` 4