import Fibonacci
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "fibNaive" $ do
    it "returns the proper fib" $ do
      all id (runChecks fibNaive) `shouldBe` True

  describe "fibLinear" $ do
    it "returns the proper fib" $ do
      all id (runChecks fibLinear) `shouldBe` True

  describe "fibLinear banged" $ do
    it "returns the proper fib" $ do
      all id (runChecks fibLinearBang) `shouldBe` True

  describe "fibClassic" $ do
    it "returns the proper fib" $ do
        all id (runChecks fibClassic) `shouldBe` True

fibSeq = [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
fibWithIdx = zip [0 .. ] fibSeq

runChecks f = do
            (idx, exptd) <- fibWithIdx
            return $ f idx == exptd