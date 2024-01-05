import CombinatorialOptimisation.TIM
import FileFormat.TIM

import System.Random
import Data.List

main :: IO()
main = do p<-loadTIMFileRaw "../exampleProblems/TT2002/competition01.tim"
          writeFile "Test.csv" $ rawToCSV p
          p'<-loadTIMFile "../exampleProblems/TT2002/competition01.tim"   
          writeFile "Test2.csv" $ viewConstrainedProblem p'
          --putStrLn $ timeTableDetailsAsCSV p'
          gen<-newStdGen
          let p'' = stochasticBasic p' gen
          putStrLn $ timeTableDetailsAsCSV p''
          print p''

          userIn<-getLine 
          let p'''=descheduleEvent (read userIn) p'' 
          putStrLn $ timeTableDetailsAsCSV p'''
          print p''
          print p'''

          -- mapM_ print $ take 2000000 $ randomWalk gen p''
          -- print $ unscheduledEvents p'

stochasticBasic t g = foldl (\c (a,b,d)->schedule a b d c) t $ zip3 randomTimes randomRooms (unscheduledEvents t)
  where
    (g1,g2) = split g
    randomTimes = randomRs (0,numberOfTimeSlots t -1) g1
    randomRooms = randomRs (0,numberOfRooms t -1) g2

perterb :: RandomGen g=>Int->g->TimeTable->TimeTable
perterb i g t = stochasticBasic finalT finalG
  where
    baseLimit = length (currentlyScheduledEvents t)-1
    (finalT,finalG) = descheduleSome i baseLimit g t 
    descheduleSome 0 _ g' t' = (t',g')
    descheduleSome n r g' t' = let (v,g'') = randomR (0,r) g'
                                   k = currentlyScheduledEvents t' !! v
                               in descheduleSome (n-1) (r-1) g'' (descheduleEvent k t')

randomWalk :: RandomGen g=>g->TimeTable->[TimeTable]
randomWalk g t = let gs = unfoldr (Just . split) g
                     as = t : zipWith f as ps
                     ps = zipWith (perterb 3) gs as
                 in as
  where
    f a b = let aV = ittcObjectiveValue a
                bV = ittcObjectiveValue b
            in if aV>bV then a else b
      
    






