-- Import necessary modules
import System.Environment
import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Char (isDigit)
import qualified Problems.NPHARD.TSP as TSP
import qualified ProbableAlgorithms.Colony.Ants as Ants
import qualified ProbableAlgorithms.Colony.Solver as Solver
import System.Random

-- Define a type for the problem instance
type Coord = (Double, Double)
type Problem = [Coord]

-- Function to read a .tsp file and extract coordinates
readTSP :: FilePath -> IO Problem
readTSP filePath = do
    content <- readFile filePath
    let -- Extract lines from the file
        linesOfFile = lines content
        -- Extract coordinates from lines
        coordinates = map parseLine $ filter isCoordLine linesOfFile
    return coordinates

-- Function to check if a line contains coordinate information
isCoordLine :: String -> Bool
isCoordLine line = case words line of
    (_:x:y:_) -> all isDigit x && all isDigit y
    _         -> False

-- Function to parse a line into coordinates
parseLine :: String -> Coord
parseLine line = case words line of
    (_:x:y:_) -> (read x, read y)
    _         -> error "Invalid line format"

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            -- Read problem instance from the specified file
            tspProblem <- readTSP filePath
            -- Print the problem instance
            putStrLn $ "Problem instance: " ++ show tspProblem
            -- Convert the problem instance to a Country
            let tspCountry = TSP.createCountry tspProblem
                rng = mkStdGen 674835
                config = Ants.AntConfig 70 1000 1 3 15 25 (`div` 2)
                (solution, _) = Solver.runSolver rng tspCountry config
            -- Print the solution or perform any other necessary action
            putStrLn solution
        _ -> putStrLn "Usage: Main <file.tsp>"





{-module Main (
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
-}    

