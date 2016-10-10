import Fibonacci
import Test.Hspec

main :: IO ()
main = hspec $ do
  let fibSeq = [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
  let fibWithIdx = zip [0 .. ] fibSeq
  describe "fibNaive" $ do
    it "returns the proper fib" $ do
      let checks = do
            (idx, exptd) <- fibWithIdx
            return $ fibNaive idx == exptd
      all id checks `shouldBe` True
  describe "fibLinear" $ do
    it "returns the proper fib" $ do
      let checks = do
              (idx, exptd) <- fibWithIdx
              return $ fibLinear idx == exptd
      all id checks `shouldBe` True
  describe "fibClassic" $ do
    it "returns the proper fib" $ do
        let checks = do
              (idx, exptd) <- fibWithIdx
              return $ fibClassic idx == exptd
        all id checks `shouldBe` True
