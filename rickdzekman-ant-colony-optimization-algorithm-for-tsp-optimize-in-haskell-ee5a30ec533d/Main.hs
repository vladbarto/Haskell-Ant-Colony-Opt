module Main (
    main
) where

import Problems.NPHARD.TSPGenerate
import ProbableAlgorithms.Colony.Ants
import ProbableAlgorithms.Colony.Solver
import System.Random

main :: IO ()
main = do
    rng <- newStdGen--let rng = mkStdGen 674835
    let (rng1,rng2) = split rng
        x = countryFromRand 1.5 rng2
        (y,_) = runSolver rng1 x $ AntConfig 70 1000 1 3 15 25 (`div` 2)
    writeGraph $ generateTemplate' x
    putStrLn y
    return ()