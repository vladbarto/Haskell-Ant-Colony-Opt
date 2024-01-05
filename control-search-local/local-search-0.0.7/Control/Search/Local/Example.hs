{-|  
  This library has embedded within it two example TSP files drawn from the TSPLIB; burma14 and fl417. 
  This module provides a loading routine for these two files only. General loading routines for 
  the TSPLIB format are provided by the Combinatorial Problems library.

  This module also provides a collection of simple TSP perturbation and recombination methods for use in the following 
  examples. Much of the code for these examples, in terms of the TSP implementation, recombination and perturbation 
  methods is not particularly efficient and only intended for example purposes. 

  To run these examples first use the following imports;
 
  > import Control.Search.Local
  > import Control.Search.Local.Example
  > import Control.Search.Local.Strategy
  > import Control.Search.Local.Eager

  * A simple maximal iterative improver. This will print out all the solutions encountered.

  > loadExampleFile BURMA14 >>= return .  loopP (maximalii (map adjacentExchangeN))

  * A stochastic choice from the improvement neighbourhood

  > iiExample  
  >    = do prob<-loadExampleFile FL417
  >         strat<-newStdGen >>= return . stochasticii rChoice . randoms 
  >         return . loopP (strat (map adjacentExchangeN)) $ prob
  > where
  >   rChoice xs p = xs !! (floor ((p::Float) * fromIntegral (length xs)))             

  * The standard TABU search, with a TABU list size of 5

  > loadExampleFile BURMA14 >>= return . bestSoFar . loopP (standardTabu 5 (map adjacentExchangeN) (map head))  

  * A more complex TABU search, with a varying neighbourhood and varying TABU list size
 
  > tabuExample 
  >    = do prob<-loadExampleFile FL417
  >         nF  <- newStdGen >>= return . stochasticNeighbourhood 417 
  >         vWin <- newStdGen >>= return . varyWindow . randomRs (5,10)
  >         return . bestSoFar . loopP (tabu (vWin . window 15) nF (map head)) $ prob

  * A simulated annealing search, using an adjacent exchange perturbation and a common geometric cooling strategy.
    The values of the cooling strategy have been selected through rather rough and ready quick testing.

  > saExample 
  >  = do prob<-loadExampleFile FL417
  >       (fIs,sIs) <- newStdGen >>= return . (\a->(map head a,map last a)) . chunk 2 . randomRs (0,numCities prob-1) 
  >       let perturb = zipWith3 swapPositions fIs sIs
  >       choiceRs <-newStdGen >>= return . randoms 
  >       return . bestSoFar . loopP (sa getTSPVal perturb 
  >                                      (geoCooling 80000 (0.99 :: Float))
  >                                      choiceRs) $ prob 

  * A genetic algorithm which only makes use of recombination. 

  > gaNoMutate 
  >  = do prob<-loadExampleFile FL417
  >       recomb<-newStdGen >>= return . stochasticRecombine
  >       startSols <- newStdGen >>= return . randomStarts 20 prob
  >       let dist = (++[1]) . takeWhile (<1) $ iterate  (*1.0884) (0.2::Float)
  >       rs <- newStdGen >>= return . randoms 
  >       return . bestSoFar . loopS (ga (makePop 20) 
  >                                      (recomb . gaSelect 2 dist rs) 
  >                                      id) $ startSols   

  * A complete genetic algorithm that mutates in a random pattern (at a rate of 1/20th)

  > gaWithMutate 
  >  = do prob<-loadExampleFile FL417
  >       recomb<-newStdGen >>= return . stochasticRecombine
  >       startSols <- newStdGen >>= return . randomStarts 20 prob
  >       pattern <- newStdGen >>= return . map (<(0.05::Float)) . randoms -- boolean pattern
  >       (fIs,sIs) <- newStdGen >>= return . (\a->(map head a,map last a)) . chunk 2 . randomRs (0,numCities prob-1) 
  >       let dist = (++[1]) . takeWhile (<1) $ iterate  (*1.0884) (0.2::Float)
  >       let mut = nest pattern (zipWith3 swapPositions fIs sIs) 
  >       rs <- newStdGen >>= return . randoms 
  >       return . bestSoFar . loopS (ga (makePop 20) 
  >                                      (recomb . gaSelect 2 dist rs) 
  >                                      mut) $ startSols  

  All these examples are best demonstrated by composition with the following limiting function, parametrised as 
  seen fit by the user;

  > strategy >>= return . limiterTSP 0 10 
-}

module Control.Search.Local.Example(
  -- * Loading routines
  ExampleFiles(FL417,BURMA14),loadExampleFile,
  -- * Perturbation functions
  swapPositions,adjacentExchange,
  -- * Neighbourhood functions
  adjacentExchangeN,stochasticNeighbourhood,
  -- * Recombination function
  stochasticRecombine,
  -- * Other TSP interaction functions
  randomStarts,getTSPVal,
  -- * Functions for terminating the search, not yet folded into the main library
  limiter,limiterTSP
)where

import Paths_local_search
import CombinatorialOptimisation.TSP
import FileFormat.TSPLIB
import Control.Search.Local
import qualified Data.IntMap as IM
import qualified Data.Set as S
import System.Random
import Data.List

data ExampleFiles = FL417 | BURMA14

{-| A stream transformation that converts a local search process into a finite list. 
    The function takes a quality function parameter, that can yield a floating point quality of a solution.
    The remaining functions control the limiting process;
  
    (1) When the difference in quality between two solutions is below the second parameter, terminate
    (2) The two solutions that we are comparing are divided by the integer parameter
 -}
limiter :: (Floating f,Ord f)=>(s->f)->f->Int->StreamT s s
limiter quality l i xs = map fst . takeWhile f $ zip xs (drop i xs)
  where
    f (a,b) = abs (quality a - quality b) > l

{-| Specialisation of limiter, fixing the quality function and the problem data type. -} 
limiterTSP = limiter getTSPVal

{-| Demonstration loading routine for only two files stored within this library. After loading this routine also 
    randomises the initial solution route.

    For more general TSP loading routines see 'FileFormat.TSPLIB'. 
-}
loadExampleFile :: ExampleFiles->IO TSPProblem
loadExampleFile FL417 = lFile "fl417.tsp"
loadExampleFile BURMA14 = lFile "burma14.tsp"

lFile :: String->IO TSPProblem
lFile s = do p<-getDataFileName s >>= loadTSPFile ExplicitMatrix 
             g<-newStdGen
             return $ randomiseRoute g p

{-| Genetic algorithms require a number of (usually) stochastically generated solutions to begin the process, not 1.
    This function is provided for these cases, taking the parameters;

    (1) the number of solutions to produce
  
    (2) a sample solution (for edgeweights and problem size)
 
    (3) a random number generator
-}
randomStarts :: RandomGen g=>Int->TSPProblem->g->[TSPProblem]
randomStarts i x g = let rs = (chunk (numCities x) . randoms $ g) :: [[Float]]
                         routes = map ((0:) . map fst . sortBy (\a b->compare (snd a) (snd b)) . zip [1 .. numCities x -1]) 
                     in take i $ map (flip setRoute x) (routes rs)


{-| Not a loading routine, but a synonym for a function within the 'CombinatorialOptimisation.TSP' library.-}
getTSPVal :: Floating f=>TSPProblem->f
getTSPVal = solutionValue

{-| A synonym for the function 'swapCitiesOnIndex' found in the 'CombinatorialOptimisation.TSP' library. 
    This will form the foundation of our perturbation and neighbourhood functions.-}
swapPositions :: Int->Int->TSPProblem->TSPProblem
swapPositions = swapCitiesOnIndex 

{-| Swap a city, indicated by index, with the city after it, indicated by index.-}
adjacentExchange :: Int->TSPProblem->TSPProblem
adjacentExchange i = swapPositions i (i+1)

{-| For a particular path, generate every path that can exist from swapping adjacent cities.-}
adjacentExchangeN :: TSPProblem->[TSPProblem]
adjacentExchangeN a = map (flip adjacentExchange a) [0 .. numCities a -2] 

{-| Many strategies benefit from a small manageable neighbourhood, but with the opportunity to access wider options.
    This stream transformer provides this, at each step providing a neighbourhood of size N, drawn randomly from the 
    set of all possible city swaps, adjacent or otherwise. 

    This does not need to be defined as a stream transformer, but the alternative still requires parametrisation 
    with values that will be drawn from a source of random numbers. This version would then require lifting to 
    become a stream transformer, and this introduces more complications in the meta-heuristic code.
-}
stochasticNeighbourhood :: RandomGen g=>Int->g->StreamT TSPProblem (List TSPProblem)
stochasticNeighbourhood nSize g sols = let as = chunk nSize . chunk 2 . randomRs (0,numCities (head sols) -1) $ g
                                           f s = map (\[a,b]->swapPositions a b s)
                                       in zipWith f sols as

{-| A recombination process, for use in the genetic algorithm examples. This is presented as a contraction, however 
    it does assume that each population has already been constrained to elements that will form the parents of the 
    new solution. This process also assumes that there will be exactly 2 parents to each new solution, so it is 
    an example of a recombination method only. -}
stochasticRecombine :: RandomGen g=>g->StreamT (List TSPProblem) TSPProblem
stochasticRecombine g a = unfoldr f (a,randoms g :: [Double])
  where
    f ([p1,p2]:ps,rs) = let (fs,rs') = stoRebuild rs (IM.fromList $ zip [0..] [(head c,last c,c) |c<-commonFragments p1edges p2route ])
                        in Just (setRoute fs p1 ,(ps,rs')) 
--unsafePerformIO (print . getTSPVal $ (setRoute fs p1))
      where
        p1edges = let as = cycle p1route in S.fromList . map fst $ zip (zip as (tail as)) p1route
        p2edges = let as = cycle p2route in S.fromList . map fst $ zip (zip as (tail as)) p2route
        p2route = IM.elems . routeMap $ p2
        p1route = IM.elems . routeMap $ p1

        stoRebuild (a:b:xs) lookup 
          | si == 1   = ((\(_,_,x)->x)  (lookup IM.! 0),a:b:xs)
          | a' == b'  = stoRebuild xs lookup 
          | S.member (aE,bS) p1edges || S.member (aE,bS) p2edges = stoRebuild xs (linkChunks a' b' (si-1) (aS,bE,a''++b'') lookup)
          | S.member (bE,aS) p1edges || S.member (bE,aS) p2edges = stoRebuild xs (linkChunks a' b' (si-1) (bS,aE,b'' ++ a'') lookup)
          | head xs <0.3                                         = stoRebuild (tail xs) (linkChunks a' b' (si-1) (aS,bE,a''++b'') lookup) 
          | head xs <0.6                                         = stoRebuild (tail xs) (linkChunks a' b' (si-1) (bS,aE,b''++a'') lookup)
          | otherwise = stoRebuild (tail xs) lookup
          where
            si = IM.size lookup
            si' = fromIntegral si
            a' = floor (a*si')
            b' = floor (b*si')
            (aS,aE,a'') = lookup IM.! a'
            (bS,bE,b'') = lookup IM.! b'
        linkChunks index1 index2 index3 newChunk lookup | index1 > index2 = linkChunks index2 index1 index3 newChunk lookup
                                                        | otherwise = IM.delete index3 . IM.insert index2 (lookup IM.! index3) . IM.insert index1 newChunk $ lookup

commonFragments :: Ord t => S.Set (t, t) -> [t] -> [[t]] 
commonFragments aEdges (b:bs) = f bs [[b]]
  where
    f [] [c] = [reverse c]
    f [] (c:cs)
      | S.member (head c,head $ last cs) aEdges = (reverse c ++ last cs) : init cs
      | otherwise = reverse c : cs
    f (x:xs) (c:cs)
      |  S.member (head c,x) aEdges = f xs ((x:c):cs)
      |  otherwise = f xs ([x] : reverse c : cs)

