import PdePreludat
import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tests Ejercicio Jurassic Park" $ do
    it "Filtro de pesados funciona bien :D" $ do
      pesados dinos `shouldBe` [trex,triceratops,omni]
    --it "Cambio de datos" $ do
     -- (dieta.cambio) dino1 `shouldBe` Dinosaurio "sa" 123 123 [Carne]

