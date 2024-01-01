module Problems.NPHARD.TSPGenerate where

import Problems.NPHARD.TSP

import System.Random
import Data.Functor
import qualified Data.Map as M
import Control.Monad.Random
import Data.List

-- Using a random number template in the form of [[(Int,Int)]] create a country with connected
-- cities which can be parsed into a GraphViz format. Does not return a new StdGen
countryFromRand :: Double -> StdGen -> Country
countryFromRand d rng = Country cityMap (edgesetFromMap cityMap)
   where
      ceilint :: Int -> Int
      ceilint x = ceiling $ d * (fromIntegral x)
      mc1 = ceilint 5
      mc2 = mc1 + 3
      es1 = 3
      es2 = ceilint es1
      ws1 = 2
      ws2 = ceilint (ws1 + 1)
      count = getRandomR (mc1,mc2)
      cityMap = evalRand randCitMap rng
         where
            randCitMap :: Rand StdGen (M.Map Id City)
            randCitMap = initialCits >>= (\x -> M.foldr foldfunc (return x) x)
            initialCits :: Rand StdGen (M.Map Id City)
            initialCits = (\y -> M.fromList [(x,City x []) | x<-[0..(y - 1)]]) <$> count
            foldfunc :: City -> Rand StdGen (M.Map Id City) -> Rand StdGen (M.Map Id City)
            foldfunc a acc = fst <$> (acc >>= \acc' -> edgeCount >>= foldl' subfold (return (acc',excludeOne acc' a)))
               where
                  edgeCount :: Rand StdGen [Int]
                  edgeCount = (\x -> [0..x]) <$> getRandomR (es1,es2)
                  subfold :: Rand StdGen (M.Map Id City,M.Map Id City) -> Int -> Rand StdGen (M.Map Id City,M.Map Id City)
                  subfold sacx _ = sacx >>= \(sacc,exc) -> wt exc >>= \wt' -> lookCon wt' sacc exc
                     where
                        l exc = M.size exc - 1
                        wt exc = getRandomR (ws1,ws2) >>= \x -> getRandomR (0,l exc) >>= \y -> return (x,y)
                        lookCon (w,x) sacc exc = return (connectEdges sacc (getId a) (getId $ M.elems exc !! x) w,M.delete (getId $ M.elems exc !! x) exc)


-- Outputs the GraphViz compatible string into a .gv file. Use this command to create a png:
-- circo -Tpng output.gv -o tspCirco.png
--writeGraph = generateTemplate' >>= writeFile "output.gv"
writeGraph :: String -> IO ()
writeGraph = writeFile "problem.gv"

-- Generate a graph viz string from a country
--generateTemplate' = generateGraphviz <$> countryFromRand <$> createCitiesWeighted <$> newStdGen
generateTemplate' :: Country -> String
generateTemplate' = generateGraphviz
