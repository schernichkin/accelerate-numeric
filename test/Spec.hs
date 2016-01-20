import           Data.AEq
import           Data.Array.Accelerate hiding ((++))
import           Data.Array.Accelerate.GaussianElimination
import           Data.Array.Accelerate.Interpreter
import           Data.Array.Accelerate.Matrix
import           Control.Monad
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding ( Test )

infix  1 @~=?

(@~=?) :: ( AEq a, Show a ) => a -> a -> Assertion
expected @~=? actual = unless (actual ~== expected) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

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
  ]

main :: IO ()
main = defaultMain
  [ gaussianEliminationTest
  , minvTest
  , mmulTest
  ]
