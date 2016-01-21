import           Data.AEq
import           Data.Array.Accelerate hiding ((++))
import           Data.Array.Accelerate.Interpreter
import           Data.Array.Accelerate.Matrix
import           Control.Monad
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding ( Test )
import           Test.QuickCheck

infix  1 @~=?

(@~=?) :: ( AEq a, Show a ) => a -> a -> Assertion
expected @~=? actual = unless (actual ~== expected) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

matrixOf :: ( Elt e ) => Int -> Int -> Gen e -> Gen (Acc (Array DIM2 e))
matrixOf m n gen = vectorOf (m * n) gen >>= return . use . fromList (Z:.m:.n)

gaussianEliminationTest :: Test
gaussianEliminationTest = testGroup "Gaussian elimination"
  [ testCase "Known linear system" $
      let a = fromList (Z:.3:.4) [ 1,  3,  1, 9
                                 , 1,  1, -1, 1
                                 , 3, 11,  5, 35
                                 ] :: Array DIM2 Float
      in [ -3, 4, 0] @~=? toList (run $ eliminate $ use a)
  ]

minvTest :: Test
minvTest = testGroup "minv"
  [ testCase "Known inverse" $
      let a = fromList (Z:.3:.3) [  2, -1,  0
                                 , -1,  2, -1
                                 ,  0, -1,  2
                                 ] :: Array DIM2 Float
      in [ 0.75, 0.5, 0.25
         ,  0.5, 1,   0.5
         , 0.25, 0.5, 0.75 ] @~=? toList (run $ minv $ use a)
   ]

mmulTest :: Test
mmulTest = testGroup "mmul" $
  [ testCase "Known matrixes" $
      let a = fromList (Z:.3:.4) [ 1, 2, 3, 4
                                 , 5, 6, 7, 8
                                 , 9, 0, 1, 2 ] :: Array DIM2 Int
          b = fromList (Z:.4:.2) [ 1, 2
                                 , 3, 4
                                 , 5, 6
                                 , 7, 8 ] :: Array DIM2 Int
      in [  50,  60
         , 114, 140
         , 28,   40 ] @=? toList (run $ mmul (use a) (use b))
  , testProperty "Associativity" $ forAll
      ( do
          a  <- choose (1, 5)
          b  <- choose (1, 5)
          c  <- choose (1, 5)
          d  <- choose (1, 5)
          m1 <- matrixOf a b (arbitrary :: Gen Int)
          m2 <- matrixOf b c (arbitrary :: Gen Int)
          m3 <- matrixOf c d (arbitrary :: Gen Int)
          return (m1, m2, m3)
      ) ( \(m1, m2, m3) -> (toList $ run $ mmul ( mmul m1 m2 ) m3)
                        == (toList $ run $ mmul m1 ( mmul m2 m3 ))
      )
  , testProperty "Scalar associativity" $ forAll
    ( do
        a  <- choose (1, 5)
        b  <- choose (1, 5)
        c  <- choose (1, 5)
        s  <- arbitrary :: Gen Int
        m1 <- matrixOf a b (arbitrary :: Gen Int)
        m2 <- matrixOf b c (arbitrary :: Gen Int)
        return (unit $ lift s, m1, m2)
    ) ( \(s, m1, m2) ->
        let r1 = toList $ run $ mscale s (mmul m1 m2)
            r2 = toList $ run $ mmul (mscale s m1) m2
            r3 = toList $ run $ mmul m1 (mscale s m2)
        in (r1 == r2) && (r2 == r3)
    )
  ]

main :: IO ()
main = defaultMain
  [ gaussianEliminationTest
  , minvTest
  , mmulTest
  ]
