-----------------------------------------------------------------------------
-- |
-- Module      :  CombinatorialOptimisation.TSP
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- A library for the representation and manipulation of travelling salesperson
-- problems.
-- The approach taken is the creation of a complex data structure called 
-- TSPProblem which contains both the problem, the current solution and 
-- the current value of the route.
-- The route is stored as a dictionary (@Data.Map@) of vertex indexes
-- to a pair of values, the previous vertex and the next vertex in the
-- sequence. This is to facilitate changing the route quickly, and
-- avoid searching for data in lists.
--
-- The data structure also contains two additional fields, the 
-- @routeElementToIndex@ and @indexToRouteElement@ components.
-- These exist to allow manipulation either by the vertex number
-- or the position in the current solution. 
-- Solutions are hamiltonian cycles.
-- For ease of reasoning it is recommended that users do not 
-- attempt to move vertex 0, or index 0, so that solutions
-- are cycles from 0 to 0. I may change this in the future to 
-- lock this down a bit. In the meantime, there is no
-- actual problem with making these changes, however 
-- later manipulations may not match up clearly with 
-- the way the show routines work.
--
-- Currently only two functions are provided for manipulating routes,
-- either by position in the sequence (@exchangeCitiesOnIndex@) or 
-- by vertex name (@exchangeCities@).
--
-- I am not sure how this will clearly support meta-heuristics that
-- work by deleting edges and recombining subsequences. However 
-- since I am storing association lists I think it should be possible 
-- to make this work, I will worry about it later.
----------------------------------------------------------------------------- 

module CombinatorialOptimisation.TSP(
  TSPProblem(TSPProblem,numCities,cityNameToIndex,indexToCityName,solutionValueI,edgeCostI),
  edgeCost,solutionValue,getTSPPathAsList,routeMap,generateDirectionalRouteMap,
  InternalStorage(ExplicitMatrix,TriangularMatrix,Recomputation),
  showEdgeWeights,
  swapCities,
  swapCitiesOnIndex,
  kFragments,allVariations,minimumVariation, stochasticReversal, shuffleFragments,
  evaluateRouteNaive,
  randomiseRoute,
  setRoute,
  makeASymmetricTSPMap,
  makeSymmetricTSPMap,
  makeEuclideanTSPMap
)where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Array as A
import System.Random
import Data.List

-- going to use a fixed point internal representation, only doing additiona afterall

import CombinatorialOptimisation.TSP.FixedPoint

{- |  The data type for carrying the combination problem and solution to 
      the TSP. The route is stored as a dictionary of associations 
      from vertex name to a pair of values, the name of the preceding 
      vertex and the next vertex. This forms an infinite loop, so 
      use carefully.

      The @routeElementToIndex@\/@indexToRouteElement@ pair store 
      fixed indexes to the cities. This is intended to allow 
      a dumb heuristic to decide to switch elements 0 and 2, 
      knowing they must be separated by 1 element, rather than
      vertices 0 and 2, which may be next to each other, or 
      very different parts of the cycle.
-}

data TSPProblem = TSPProblem { solutionValueI :: FP,
                               edgeCostI :: (Int->Int->FP),
                               numCities :: Int,
                               cityNameToIndex :: IM.IntMap Int,
                               indexToCityName :: IM.IntMap Int
                             }

cityAtIndex :: TSPProblem->Int->Int
cityAtIndex t i | i>=numCities t = cityAtIndex t (i-numCities t)
                | otherwise = indexToCityName t IM.! i

{- |  Will generate an IntMap of the path, giving city to next and last city names as the structure.
      This is effectively the edges involved in the process. Wow, how badly written was that. -}
generateDirectionalRouteMap :: TSPProblem->IM.IntMap (Int,Int)
generateDirectionalRouteMap t | null path        = IM.fromList []
                              | length path == 1 = IM.fromList [(head path,(head path,head path))]
                              | otherwise        = IM.fromList path'
  where
    path = IM.elems . routeMap $ t
    end = last path
    fr = (head path,(end,path !! 1))
    bk = (end,(last . init $ path,head path))
    path' = fr:[(b,(a,c)) | (a,b,c)<-zip3 path (tail path) (tail $ tail path)]++[bk]

{- |  The definition of equality is quite slow, it will check the internal representation of the path that 
the solution follows. -}
instance Eq TSPProblem where
  a == b = cityNameToIndex a == cityNameToIndex b 

{- | The definition of orderability is much faster, coming down to a simple integer comparison.-}
instance Ord TSPProblem where
  compare a b = compare (solutionValueI a) (solutionValueI b)

routeMap = indexToCityName

{- | Internally the value of the solution is stored as a fixed point value, stored in an Int64 data type. 
     For external visibility however this function is provided, converting the value into a floating point numbers. 
-}

solutionValue :: Floating a=>TSPProblem->a
solutionValue = realToFrac . solutionValueI

{- | Internally all edge costs are stored as a fixed point values. 
     For external visibility however this function is provided, converting the values into floating point numbers. 
-}

edgeCost :: Floating a=>TSPProblem->Int->Int->a
edgeCost t a b = realToFrac $ edgeCostI t a b

{- |  There are three possible internal storage forms. A full explicit matrix, an upper triangular matrix or recomputation 
      from data points. The advantage of full explicit is speed, but it takes more memory. It is also the only option for 
      asymmetric TSP problems. The triangular matrix is also fast, but can only be used in symmetric problems, and also 
      still requires quite a bit of memory. Recomputation is the last option, it is slow because it is no longer a lookup
      table, but will take much less room. Can only be used with problems where the distance between two points can be
      calculated. Currently I am only supporting symmetric TSPs for this.
-}

data InternalStorage = ExplicitMatrix | TriangularMatrix | Recomputation deriving (Show,Eq) -- just in case I need these

instance Show TSPProblem where
  show t = concat ["TSPProblem of ",show . numCities $ t,
                   " cities\n    Current Solution ",show . getTSPPathAsList 0 $ t,
                   "\n    Costing ",show . solutionValue $ t,"\n"]

getTSPPathAsList :: Int->TSPProblem->[Int]
getTSPPathAsList alignOn t = let (as,bs) = span (/=alignOn) . IM.elems . routeMap $ t in bs++as
                     
{- |  Converts the lookup table of a problem into a comma and newline delimited
      string. This should facilitate copying into spreadsheets for checking the 
      problem being used and validating solutions by hand. -}

showEdgeWeights :: TSPProblem->String
showEdgeWeights t = headerRow ++ concatMap makeRow nc
  where
    ep = edgeCost t
    nc = [0 .. numCities t-1]
    headerRow = ',': concat (intersperse "," $ map show [0..numCities t-1]) ++ "\n"
    makeRow i = show i ++ "," ++ concat (intersperse "," [ show (ep i' i) |  i'<-nc]) ++"\n"

{- |  Will perform a switch of 2 cities in the path. This is by city name, not current index
      in the path. It looks up the current indexes by city name and passes the work off to 
      @exchangeCitiesOnIndex@.  -}

swapCities :: Int->Int->TSPProblem->TSPProblem
swapCities a b t = swapCitiesOnIndex i1 i2 t 
  where
    i1 = cityNameToIndex t IM.! a
    i2 = cityNameToIndex t IM.! b
 
{- |  Performs the bulk of the work for exchanging elements of the cycle.
      This version no longer assumes the indices are ordered due to confusion this
      caused in my own code. In addition there was an oversight, when exchanging indices
      0 and last. This is now fixed.
 -}

swapCitiesOnIndex :: Int->Int->TSPProblem->TSPProblem
swapCitiesOnIndex i1 i2 t 
  | i1 > i2 = swapCitiesOnIndex i2 i1 t
  | d == 0 = t
  | d == nc = t{solutionValueI=solutionValueI t + priceChangeAdjC,
                          cityNameToIndex=cnti,
                          indexToCityName = itcn}  -- adjacent wrap around
  | d == 1   = t{solutionValueI=solutionValueI t + priceChangeAdjA,
                          cityNameToIndex=cnti,
                          indexToCityName = itcn}  -- adjacent indexes
  | otherwise = t{solutionValueI=solutionValueI t + priceChangeAdjB,
                          cityNameToIndex=cnti,
                          indexToCityName = itcn}
  where 
    d = i2 - i1
	
    -- basic setup (common code)
    t1 = indexToCityName t
    a = t1 IM.! i1
    b = t1 IM.! i2
    p = edgeCostI t
    nc = numCities t -1

    -- index exchange (common code)
    itcn = IM.insert i2 a (IM.insert i1 b t1)
    cnti = IM.insert b i1 (IM.insert a i2 $ cityNameToIndex t)      

    -- find related cities
    a1 = if i1 == 0 then t1 IM.! nc  else t1 IM.! (i1 -1) 
    a2 = t1 IM.! (i1 +1)
    b1 = t1 IM.! (i2 -1)
    b2 = if i2 == nc then t1 IM.! 0 else t1 IM.! (i2 +1) 

    -- pricing codes
    priceChangeAdjB = sum [p a1 b,p b a2,p b1 a,p a b2] - sum [p a a2,p b b2,p a1 a,p b1 b] 
    priceChangeAdjA = sum [p a1 b,p b a,p a b2] - sum [p a1 a,p a b,p b b2]  
    priceChangeAdjC = sum [p b1 a,p a b,p b a2] - sum [p b1 b,p b a,p a a2]  

{- |  A brute force recalculation of the current length of the path. Use sparingly.-}

evaluateRouteNaive :: TSPProblem->TSPProblem
evaluateRouteNaive t = t{solutionValueI=sum' 0 rm (tail $ cycle rm)}
   -- . zipWith ep rm $ (tail $ cycle rm)} -- evalRoute rm}
  where
    ep = edgeCostI t
    rm = IM.elems (routeMap t)
    sum' v [x] (y:_) = v+ep x y
    sum' v (x:xs) (y:ys) = let n = ep x y + v in n `seq` sum' n xs ys
                          
   
{- |  Take a path through the system and a problem, insert the path into the system, 
      calculating distances and setting up appropriate look up tables. It does not
      validate the list in terms of going through all the cities, or going through 
      a city more than once (though this is likely to break other parts of the system 
      very very fast). It does organise the list so that the starting node is vertex 0. 

      Uses the @evaluateRouteNaive@ to calculate the length of the path via a brute
      force method. This is not expected to be used frequently. -}  

setRoute :: [Int]->TSPProblem->TSPProblem
setRoute path t = evaluateRouteNaive t{indexToCityName=in1,cityNameToIndex=in2} 
  where
    in1 = IM.fromList $ zip [0..] path
    in2 = IM.fromList $ zip path [0..]

{- |  An implementation of kexchange methods as a series of combinators. kFragments
      will break a problem at specified edges and give back the path segments. 
      allVariations will take a set of segments and give back every possible combination of them, 
      and their reversals (I feel this should be broken down further, due to similarities with the 
      stochastic segment reversal method). allVariations is also a more general function than 
      the others, not actually requiring reference to TSP. 
      minimumVariation is expected to be used with a set of paths (probably generated by the above two methods)
      and give only the best.
      
      stochasticReversal and shuffleFragments are similarly not really approapriate just here, but for now they 
      stay. Together they allow for less detailed k-exchange methods, for example, rather than exhaustively creating 
      variations, we will use it as a mutator for a GA. For this we might want to only create 1 permutation, which 
      can be done as; kFragments >>> shuffleFragments >>> stochasticReversal >>> \as->[concat as] >>> minimumVariation

      kFragments will begin by being quite fragile. Please make sure that your input sequence is 
      assending and does not include duplicates. Remember to initialise your route before calling this. -}

kFragments :: [Int]->TSPProblem->[[Int]]
kFragments (first:xs) t = (\(a,b)->a:b) $ foldl f (basePath,[])  xs
  where
    basePath = getTSPPathAsList (cityAtIndex t (first +1)) t
    f (b,col) x = let c = cityAtIndex t (x+1)
                  in (dropWhile (/=c) b,takeWhile (/=c) b:col)
    
allVariations :: Bool->[[Int]]->[[Int]]
allVariations reversalAllowed fragments
  | reversalAllowed = rebuildR fragments []
  | otherwise       = rebuild fragments []
  where
    -- rebuildR :: [[Int]]->[Int]->[TSPProblem]
    rebuild [] xs = [xs]
    rebuild (x:xs) [] = rebuild xs x
    rebuild xs cp = concat [ rebuild ys (x++cp) | (x,ys)<-dropEach xs]
    rebuildR [] xs = [xs]
    rebuildR (x:xs) [] = rebuildR xs x ++ rebuildR xs (reverse x)
    rebuildR xs cp = concat [ rebuildR ys (x++cp) ++ rebuildR ys (reverse x ++cp) | (x,ys)<-dropEach xs] 
    dropEach xs = f ([],xs)
      where
        f (as,b:[]) = (b,reverse as):[]
        f (as,b:bs) = (b,reverse as ++ bs) : f (b:as,bs)
  
minimumVariation :: TSPProblem->[[Int]]->TSPProblem
minimumVariation t = minimum . map (flip setRoute t) 

shuffleFragments :: Ord a=>[a]->[[Int]]->[[Int]]
shuffleFragments as = map snd . sortBy (\a b->compare (fst a) (fst b)) . zip as

stochasticReversal :: [Bool]->[[Int]]->[[Int]]
stochasticReversal as = zipWith (\a b->if a then reverse b else b) as

{- |  Shuffles a simple list of cities and then passes off the work to setRoute. -}

randomiseRoute :: RandomGen g=>g->TSPProblem->TSPProblem
randomiseRoute g t = setRoute (0:map snd (sort (zip (randoms g :: [Float]) [1 .. numCities t -1]))) t  

{- |  Construct a TSPProblem instance for an Asymmetric TSP. That is, the distance
      from A-B is the not necessarily the same as B-A. The actual route will 
      not be set up initially, the dictionaries will be empty. This could be 
      used directly for a global search system (branch and bound), or use in 
      conjunction with @setRoute@ or @randomiseRoute@ to initialise for local search. 
      Internal data structure is always fully explicit matrix.-}

makeASymmetricTSPMap :: RandomGen g=>(Double,Double)->Int->g->TSPProblem
makeASymmetricTSPMap distanceLimits numCities g 
  = let cities = [0 ..(numCities-1)]
        cityCoords = [(a,b) | a<-cities,b<-cities,a/=b]
        matrix = M.fromList $ zip cityCoords (map realToFrac $ randomRs distanceLimits g)
        -- p' = (\x y->M.findWithDefault 0 (x,y) matrix)
        explicit = A.listArray (0,numCities*numCities-1)  [M.findWithDefault 0 (a,b) matrix | a<-cities,b<-cities]
    in TSPProblem 0 (\x y->explicit A.! (y * numCities + x)) numCities IM.empty IM.empty
    -- TSPProblem 0 M.empty p numCities M.empty M.empty

{- |  Construct a TSPProblem instance for a Symmetric TSP. That is, the distance
      from A-B is the same as B-A. The actual route will not be set up initially,
      the dictionaries will be empty. This could be used directly for a global 
      search system (branch and bound), or use in conjunction with @setRoute@ or 
      @randomiseRoute@ to initialise for local search. Should be noted that this
      does not create locations and calculate distances, but rather randomly 
      assigns distances to each edge, making them symmetric. -}

makeSymmetricTSPMap :: RandomGen g=>InternalStorage->(Double,Double)->Int->g->TSPProblem
makeSymmetricTSPMap Recomputation _ _ _ = error "Cannot support recomputation, please use alternative storage, or makeEuclideanTSPMap"
makeSymmetricTSPMap storageType distanceLimits numCities g 
  = let cities = [0 ..(numCities-1)]
        cityCoords = [(a,b) | a<-cities,b<-take (a+1) cities,a/=b ]
        f e ((a,b),c) = M.insert (b,a) c (M.insert (a,b) c e)
        matrix = foldl f M.empty (zip cityCoords (map realToFrac $ randomRs distanceLimits g))
        explicit = A.listArray (0,numCities*numCities-1)  [M.findWithDefault 0 (a,b) matrix | a<-cities,b<-cities]
        triangular = A.listArray (0,sum [0..numCities])  [M.findWithDefault 0 (a,b) matrix | a<-cities,b<-[0..a]]
        p = if storageType == ExplicitMatrix then (\x y->explicit A.! (y * numCities + x))
                                             else (\x y->let x' = min x y; y' = max x y in triangular A.! (div (y'*y'+y') 2 + x'))
    in TSPProblem 0 p numCities IM.empty IM.empty

{- |  Construct a TSPProblem instance for a Symmetric TSP. The route will not be
      initially set up, the dictionaries will be empty. This does create the 
      vertices of the graph as points in a 2d space, and the lengths of edges 
      are calculated, so this supports all internal storage types. 
-}

makeEuclideanTSPMap :: RandomGen g=>InternalStorage->(Double,Double)->(Double,Double)->Int->g->TSPProblem
makeEuclideanTSPMap storageType xRange yRange numCities g 
  = let cities = [0 ..(numCities-1)]
        (genA,genB) = split g
        positions = take numCities $ zip (randomRs xRange genA) (randomRs yRange genB)
        posArr = A.listArray (0 , numCities-1) positions

        explicit = A.listArray (0,numCities*numCities-1)  [euclidianDistance (posArr A.! a) (posArr A.! b) | a<-cities,b<-cities]
        triangular = A.listArray (0,sum [0..numCities])  [euclidianDistance (posArr A.! a) (posArr A.! b) | a<-cities,b<-[0..a]]

        p = case storageType of
              ExplicitMatrix -> \x y->explicit A.! (y * numCities + x)
              TriangularMatrix -> (\x y->let x' = min x y; y' = max x y in triangular A.! (div (y'*y'+y') 2 + x'))
              Recomputation -> \a b->if a == b then 0 else euclidianDistance (posArr A.! a) (posArr A.! b)
    in TSPProblem 0 p numCities IM.empty IM.empty
  where
    euclidianDistance :: (Double,Double)->(Double,Double)->FP
    euclidianDistance (a,b) (c,d) = realToFrac $ sqrt ((a-c)*(a-c)+(b-d)*(b-d))
        


