import FileFormat.TSPLIB
import CombinatorialOptimisation.TSP
import System.Random


main = do g<-newStdGen
          let prob = makeEuclideanTSPMap ExplicitMatrix (0,100) (0,100) 8 g
          let b = setRoute [0..7] prob
          print b
          print $ kFragments [2,4,7] b
          print $ exhaustiveFragmentRebuild True b . kFragments [2,4,7] $ b



{- x <- loadTSPFile TriangularMatrix "fl417.tsp"
          -- print $ showEdgeWeights x
          let x' = randomiseRoute (mkStdGen 417) x
          print $ (\(TSPProblem m _ _ _ _ _)->m) x' -}

