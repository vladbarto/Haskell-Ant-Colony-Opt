-- don't know how to use small check at this point, going to go with an error based approach.

import FixedPoint
import System.Random

main :: IO()
main = do gen1<-newStdGen
          let testGroup1 = makeTestGroup 500 gen1
          gen2<-newStdGen
          let testGroup2 = makeTestGroup 500 gen2
          print "SUMMATIONS"
          print $ ("Floats",sum testGroup1)
          print $ ("Doubles",sum .con' $ testGroup1)
          print $ ("FPs",sum . con $ testGroup1)
          putStrLn ""
          print "RECIPROCALS"
          print $ ("Floats",sum $ map recip testGroup1)
          print $ ("Doubles",sum $ map recip $ con' testGroup1)
          print $ ("FPs",sum $ map recip $ con testGroup1)
          putStrLn ""
          print "SUM DIFF"
          print $ ("Floats",sum $ zipWith (-) testGroup1 testGroup2)
          print $ ("Doubles",sum $ zipWith (-) (con' testGroup1) (con' testGroup2))
          print $ ("FPs",sum $ zipWith (-) (con testGroup1) (con testGroup2))
          putStrLn ""
          print "SUM DIV"
          print $ ("Floats",sum $ zipWith (/) testGroup1 testGroup2)
          print $ ("Doubles",sum $ zipWith (/) (con' testGroup1) (con' testGroup2))
          print $ ("FPs",sum $ zipWith (/) (con testGroup1) (con testGroup2))
          putStrLn ""
          print "SUM MUL"
          print $ ("Floats",sum $ zipWith (*) testGroup1 testGroup2)
          print $ ("Doubles",sum $ zipWith (*) (con' testGroup1) (con' testGroup2))
          print $ ("FPs",sum $ zipWith (*) (con testGroup1) (con testGroup2))


makeTestGroup :: RandomGen g=>Int->g->[Float]
makeTestGroup s g = take s $ randomRs (0,500) g

con :: [Float]->[FP]
con = map realToFrac

con' :: [Float]->[Double]
con' = map realToFrac
