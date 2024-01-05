{-| A collection of common strategies, built out of the combinators in the other libraries. 
    For examples of their use, see "Control.Search.Local.Example". ACO is the least well thought through.
-}

module Control.Search.Local.Strategies(
  -- * Iterative Improvers
  iterativeImprover,firstFoundii,maximalii,minimalii,stochasticii,
  -- * TABU Search
  tabu,standardTabu,tabuFilter,
  -- * Simulated Annealing
  sa,logCooling,linCooling,geoCooling,saChoose,
  -- * Genetic Algorithms
  ga,
  -- * Ant Colony Optimisation
  aco,pheromoneDegrade
)where

import Control.Search.Local
import Control.Search.Local.Distributions
import Data.List
import System.Random
import System.IO.Unsafe

{-|
  The generic skeleton of iterative improvers. The first parameters is a neighbourhood stream expander, 
  the second is a stream contractor which makes choices from neighbourhoods. All neighbourhoods will be
  filtered so that the elements can only improve upon the previous solution. 

  Since the parameters are stream transformers, simple functions must be lifted before they can be used 
  as parameters. For example a deterministic neighbourhood function @df@ should be lifted with @map@ and 
  to choose the first element from each improving neighbourhood you would use @map head@, giving
 
 > iterativeImprover (map df) (map head). 

  This skeleton provides a standard infinite stream of solutions, rather than terminating 
  when a local minima is reached. This provides better safety for composition than the 
  versions suggested in the paper. When the filter results in an empty list, the seed 
  value is wrapped up as a list and returned in its place.
-}
iterativeImprover :: Optimisable s=>StreamT s (List s)->StreamT (List s) s->StreamT s s
iterativeImprover nf cf =  cf . mkSafe (improvement nf)
  where
    mkSafe f sols = zipWith (\a b->if null a then [b] else a) (f sols) sols 


{-| First found iterative improvement strategy. Fixes the choice function to @map head@. -}
firstFoundii :: Optimisable s=>StreamT s (List s)->StreamT s s
firstFoundii nf = iterativeImprover nf (map head)
{-| Maximal iterative improvement strategy. Since we seek the lowest possible value of solutions this 
    translates to fixing the choice function to @map worst@. -}
maximalii :: Optimisable s=>StreamT s (List s)->StreamT s s
maximalii nf = iterativeImprover nf (map worst)
{-| Minimal iterative improvement strategy. Fixes the choice function to @map best@.-}
minimalii :: Optimisable s=>StreamT s (List s)->StreamT s s
minimalii nf = iterativeImprover nf (map best)

{-| Stochastic iterative improvement strategy. The choice function is required to make a random choice from 
    the neighbourhood at each step. This is implemented in terms of the 'select' function, and 
    uses a 'uniformCDF'. 
-}
stochasticii :: Optimisable s=>StreamT s (List s)->StreamT s s
stochasticii nf = iterativeImprover nf (select uniformCDF)


{-| -}
tabuFilter :: Eq s => (StreamT s (List s))  -- window
                   -> (StreamT s (List s))  -- neighbourhood
                   -> (StreamT s (List s))
tabuFilter wF nF xs = safe (map (:[]) xs)
                    $ zipWith (\ws->filter (flip notElem ws)) (wF xs) (nF xs)

{-| A general skeleton for TABU search. The three parameters are 

    (1) a stream transformer to create the stream of TABU lists (typically provided by 'window')

    (2) a stream transformer to create the stream of neighbourhoods, in the same manner as seen in iterative improver

    (3) a choice transformer to choose a single element from a pruned neighbourhood.
-}
tabu :: Ord s=>  StreamT s (List s)->StreamT s (List s)->
                 StreamT (List s) s->StreamT s s
tabu wf nf cf = cf . tabuFilter wf nf 

{-| Commonly TABU search does not take a function which makes a TABU list, but rather a size of 
    TABU list. We provide this less flexible form here, where the first parameter changes from 
    to being the window size. The choice function is set to /map head/. Implemented in terms of 'tabu'. 

    I am not happy with this. What is really needed is a more flexible version of 'safe', so that 
    rather than just returning the singleton it can return an alternative transformation of the neighbourhood.
    This is also some scope for using compiler rules here to balance usability with performance. 
    -}
standardTabu :: Ord s=>  Int->StreamT s (List s)->StreamT s s
standardTabu winSize nF = tabu (window winSize) nF (map head)

-- tabu cF wF nF xs = let nF' = const (nF xs)
--                    in cF . safe (tabuFilter wF nF' xs) ◦ improvement nF' $ xs


{-| A logarithmic cooling strategy intended for use within simulated annealing. Broadly the first value is 
    the starting temperature and the second a value between 0 and 1. -}
logCooling :: Floating b=>b->b->[b]
logCooling c d = map (\t->c / (log (t + d))) (iterate (+1) 1)

{-| The most common cooling strategy for simulated annealing, geometric. The first value is the starting temperature, 
    the second a value between 0 and 1, the cooling rate.  -}
geoCooling :: Floating b=>b->b->[b]
geoCooling startTemp tempChange = iterate (* tempChange) startTemp

{-| Included for completeness, this is a cooling strategy for simulated annealing that is usually not very effective,
    a linear changing strategy. The first value is the starting temperature the second is the value to increase it by 
    at each step. In order to have it reduce at each step, pass a negative value. 
-}
linCooling :: Floating b=>b->b->[b]
linCooling startTemp tempChange= iterate (+ tempChange) startTemp

{-| The traditional choice function used within simulated annealing. The parameters are; 
    a function to yield quality of a solution, a value between 0 and 1 (stochastic expected) a temperature, 
    the old solution and the possible future solution. Frustratingly this does not make use of 'Optimisable'
    because it requires the actual floating point quality values of solutions.-}
saChoose :: (Floating v,Ord v)=>(s->v)->v->v->s->s->s
saChoose valueF r t oldSol newSol
  | d<=0 || e>r = newSol
  | otherwise = oldSol
  where
    e = exp (- (d/t))
    d = (valueF newSol) - (valueF oldSol)

{-| Simulated Annealing skeleton. This requires a significant number of parameters due to the 
    various stochastic components, temperatures and the need for a numerical valuation of 
    solutions qualities. The parameters are;
   
    (1) a function to get the numerical value of a candidate solution
  
    (2) a function to provide a perturbation of a solution, with respect to some external factor, 
        such as a random number, which is what the data type /r/ is expected (though not required) to be.

    (3) a stream of values representing the temperature or cooling strategy
  
    (4) a stream of stochastic values

    (5) a stream of (stochastic) values for the creation of perturbations
-}
sa :: (Floating v,Ord v)=>(s->v)->StreamT s s->Stream v->Stream v->StreamT s s
sa getVal perturbF rs coolS sols = zipWith4 (saChoose getVal) rs coolS sols (perturbF sols)

{-| -}
ga :: Int                -- population size
   ->Float               -- mutation rate
   ->StreamT (List s) s  -- recombine, contraction
   ->StreamT (List s) (List s)   -- selection of parents 
   ->StreamT s s         -- mutation
   ->StreamT s s
ga popSize perturbProb recombine selection perturb
  = nest [(True, perturb), (False, id)] mutGo 
  . concat 
  . doMany popSize (recombine . selection) 
  . chunk popSize
  where
    mutGo = unsafePerformIO (newStdGen >>= return . map (<perturbProb) . randoms)

{-| A simple ACO implementation. It assumes that Ants will be released in groups, which is represented as the 
    population size. It requires a transformation for generating pheromones, and creating new solutions from 
    pheromone data. This will be problem specific. -}
aco :: Int -- population size
    -> (StreamT (List s) a)   -- Pheromone analysis
    -> (StreamT a s)  -- solution generation
    -> StreamT s  s
aco popSize aP gN = concat . doMany popSize gN . aP . chunk popSize

{-| ACO's often use a degrading system, so that the next trail contains some part of the previous trails.
    This function provides this functionality, assuming that pheromone data can be summed like a number, and 
    an initial state is provided. The stream transformation parameter represents a streamed 
    degrade, for example;
   
    > map (*0.1)
   
    would give one tenth of the previous previous pheromone data added to the result. This is a stream transformation to allow for flexibility, for example adding a stochastic element.  -}
pheromoneDegrade :: Num a=>a->StreamT a a->StreamT a a
pheromoneDegrade p0 degrade as = let newTrails = zipWith (+) (degrade (p0 : newTrails)) as in newTrails 






