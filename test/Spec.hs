import           Data.Array.Accelerate hiding ((++))
import           Data.Array.Accelerate.Interpreter
import           Data.Array.Accelerate.GaussianElimination
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding ( Test )

gaussianEliminationTest :: Test
gaussianEliminationTest = testGroup "Gaussian elimination"
  [ testCase "known problem 1" $
      let arr0 = fromList (Z:.3:.4) [ 1,  3,  1, 9
                                    , 1,  1, -1, 1
                                    , 3, 11,  5, 35
                                    ] :: Array DIM2 Float
          arr = run $ eliminate (use arr0)
      in [ -3, 4, 0] @=? toList arr

  ]

main :: IO ()
main = defaultMain
  [ gaussianEliminationTest
  ]
