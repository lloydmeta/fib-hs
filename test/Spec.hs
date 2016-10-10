import Fibonacci
import Test.Hspec

main :: IO ()
main = hspec $ do
  let fibSeq = [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
  let fibWithIdx = zip [0 .. ] fibSeq
  describe "fibNaive" $ do
    it "returns the proper fib" $ do
      let list = [(exptd, fibNaive idx) | (idx, exptd) <- fibWithIdx ]
      all (\(exptd, obs) -> exptd == obs) list `shouldBe` True
  describe "fibLinear" $ do
    it "returns the proper fib" $ do
      let list = [(exptd, fibLinear idx) | (idx, exptd) <- fibWithIdx ]
      all (\(exptd, obs) -> exptd == obs) list `shouldBe` True
  describe "fibClassic" $ do
    it "returns the proper fib" $ do
      let list = [(exptd, fibClassic idx) | (idx, exptd) <- fibWithIdx ]
      all (\(exptd, obs) -> exptd == obs) list `shouldBe` True
