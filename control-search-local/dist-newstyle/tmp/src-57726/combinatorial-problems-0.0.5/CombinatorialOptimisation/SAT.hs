-----------------------------------------------------------------------------
-- |
-- Module      :  CombinatorialOptimisation.SAT
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- A library for the representation and manipulation of satisfiability problems.
-- Currently this is expected to only be 3-SAT however I do not think the 
-- code is particularly limited to 3-SAT. The approach taken is that there
-- is a complex data structure called SATProblem, which contains both the 
-- problem and the solution (settings of variables). In addition it contains 
-- a number additional fields that allow for making changes quickly, such 
-- as a table of clause positions. This is a Map from clause index to the 
-- number of variable terms that are currently set to true. 
--
-- Currently the only function for quickly changing a problem is the flipping 
-- of a single variable. I think some other low level operations for finding 
-- clauses not currently evaluating to true and so on would be useful.
----------------------------------------------------------------------------- 

{-# LANGUAGE ScopedTypeVariables #-}

module CombinatorialOptimisation.SAT(
  SATProblem(SATProblem,numClauses,numSATEDClauses,numVariables,variableLookUp,variablePosition,clausePosition,clauseLookUp),
  numUnSATEDClauses,getTrueFalseCount,summariseSAT,makeRandomSATProblem,flipVariable,satproblem,setAllVars,randomiseVariables
)where

import qualified Data.IntMap as IM
import qualified Data.Array as A
import Data.List
import System.Random
import System.IO.Unsafe
import Data.Char

data SATProblem = SATProblem { numClauses :: Int,
                               numSATEDClauses :: Int,
                               numVariables :: Int,
                               variableLookUp :: Int->([Int],[Int]),
                               clauseLookUp :: Int->([Int],[Int]),
                               variablePosition :: IM.IntMap Bool,
                               clausePosition :: IM.IntMap Int}

instance Eq SATProblem where
  (==) s1 s2 = (numSATEDClauses s1) == (numSATEDClauses s2) && (variablePosition s1) == (variablePosition s2)

instance Ord SATProblem where
  compare s1 s2 = compare (numSATEDClauses s2) (numSATEDClauses s1)

instance Show SATProblem where
  show s = showSATLogic s ++"\n"++ showVARPosition s ++"\n"++ summariseSAT s++"\n"++(show $ getTrueFalseCount s)

{- |  For the purposes of getting a general impression of the state of the system, 
      it returns the number of variables in the True, and False positions. -}

getTrueFalseCount :: SATProblem->(Int,Int)
getTrueFalseCount s = let ls = IM.elems $ variablePosition s
                      in (length (filter (==True) ls),length (filter (==False) ls))

{- |  The number of unsatisfied clauses in the problem, the inverse of @numSATEDClauses@ -}

numUnSATEDClauses :: SATProblem->Int
numUnSATEDClauses s = numClauses s - numSATEDClauses s

{- |  Partial display function, for usage in show, this displays the logic of the problem. -}

showSATLogic :: SATProblem->String
showSATLogic s = concat (intersperse " /\\\n" (map writeClause [0 .. numClauses s -1])) ++ "\n"
  where
    writeClause c = let (as,bs) = clauseLookUp s c 
                        (as',bs') = (map (\a->(a,' ')) as,map (\a->(a,'!')) bs)
                        cs = sortBy (\a b->compare (fst a) (fst b)) $ as' ++ bs'
                    in '(' : (concat $ intersperse " \\/ " $ [ d :'x':show c  | (c,d)<-cs]) ++ ")"

{- |  Partial display function, for usage in show, displays some general statistics about 
      the solution status. -}

summariseSAT :: SATProblem->String
summariseSAT s = concat ["number of clauses : ",show (numClauses s),"\n",
                         "number of variables : ",show (numVariables s),"\n",
                         "satisfied clauses : ",show (numSATEDClauses s),"\n", 
                         satMessage,"\n"]
  where
    sat = (numSATEDClauses s) == (numClauses s)
    satMessage = if sat then "SATisfied" else "unSATisfied"

{- |  Partial display function, for usage in show, displays the setting of each variable. -}

showVARPosition :: SATProblem->String
showVARPosition s = concat [concat ["  x",show v," = ",show t,"\n" ]   |(v,t)<- IM.assocs (variablePosition s)]

{- |  Alternative constructor for the data structure. Takes only those elements that can 
      not be derived and correctly initialises the other components, such as calculating 
      how many clauses are currently evaluating to true. Requires the number of clauses,
      the number of variables, the lookup function for variables (variable index 
      returning two lists, the first is the indexes of clauses in which this variable 
      is present, the second list the indexes of clauses in which the inverse of this variable 
      is present), the lookup table for clauses (clause index to lists of variable indexes) 
      and the current settings of each variable. -}

satproblem :: Int->Int->(Int->([Int],[Int]))->(Int->([Int],[Int]))->IM.IntMap Bool->SATProblem
satproblem nClauses nVars varLookup claLookup varPosition
  = SATProblem nClauses satClause nVars varLookup claLookup varPosition finalClausePosition
  where
    varList = [0 .. nVars -1]
    initialClausePositions = IM.fromList $ zip [0 .. nClauses -1] $ repeat 0 
    finalClausePosition = foldl f IM.empty [0 .. nVars -1]
    f m v = let (ords,negs) = varLookup v
                varPos = varPosition IM.! v
            in if varPos then foldl (\m' c->IM.adjust (+1) c m') m ords else foldl (\m' c->IM.adjust (+1) c m') m negs
    satClause = sum $ map (\x->if x ==0 then 0 else 1) (IM.elems finalClausePosition)

{- |  For rapid initialisation of problem instances. This fixes the setting of 
      all variables to either true or false. The effect this has on the number 
      of clauses that evaluate to true is unknown until it is carried out. -}

setAllVars :: Bool->SATProblem->SATProblem
setAllVars b s = satproblem (numClauses s) (numVariables s) (variableLookUp s) (clauseLookUp s) initialVarPosition
  where
    initialVarPosition = IM.fromList $ zip [0 .. numVariables s -1] $ repeat b

{- |  For rapid initialisation of problem instances for usage in stochastic algorithms. 
      Specifically expected to be used for genetic algorithms and other forms of 
      stochastic meta-heuristic. -}

randomiseVariables :: RandomGen g=>g->SATProblem->SATProblem
randomiseVariables g s = satproblem (numClauses s) (numVariables s) (variableLookUp s) (clauseLookUp s) varpos  
  where
    varpos = IM.fromList $ zip [0 .. (numVariables s) -1] $ (randoms g :: [Bool])

{- |  I am not sure how often this will be used in practice, as randomly created problems
      often seem to be quite easy to solve. Requires a source of random numbers, the number
      of variables and the number of clauses to create, in that order. It is assumed 
      that 3-SAT problems are the type wanted. -} 

makeRandomSATProblem :: RandomGen g=>g->Int->Int->SATProblem
makeRandomSATProblem gen numVariables numClauses 
  = satproblem numClauses numVariables varLookup claLookup initialVarPosition 
  where
    initialVarPosition = IM.fromList $ zip [0 .. numVariables -1] $ repeat False
    clauses = take numClauses $ nub (unfoldr generateRandomClause gen)
    generateRandomClause g = let f (ms,ns) gen'
                                   | length ms + length ns == 3 = (ms,ns,gen')
                                   | otherwise = let (l :: Int,gen'') = randomR (0,1) gen' 
                                                     (n :: Int,gen''') = randomR (0,numVariables -1) gen''
                                                     already = elem n ms || elem n ns
                                                 in if already then f (ms,ns) gen'''
                                                               else if l ==0 then f (n:ms,ns) gen'''
                                                                             else f (ms,n:ns) gen'''
                                 (ords,negs,g') = f ([],[]) g
                             in Just ((sort ords,sort negs),g')
    emptyClauseData = IM.fromList $ zip [0 .. numVariables -1] (repeat ([],[]))
    basicClauseLookup = foldl constructClauseLookup emptyClauseData $ zip [0..] clauses
    varLookup = ((A.listArray (0,numVariables-1) (IM.elems basicClauseLookup)) A.!)
    constructClauseLookup m (clauseIndex,(ords,negs)) = let addNeg m' x = IM.adjust (\(as,bs)->(as,clauseIndex:bs)) x m'
                                                            addOrd m' x = IM.adjust (\(as,bs)->(clauseIndex:as,bs)) x m'
                                                        in foldl addNeg (foldl addOrd  m  ords) negs
    claLookup = ((A.listArray (0,numVariables-1) clauses) A.!)

{- |  The first low level operation. Takes a problem and changes the 
      setting of the indexed variable from true to false. This is 
      expected to be used in conjunction with other program logic
      to select which index to flip. -}
                                                        
flipVariable :: Int->SATProblem->(SATProblem,Int)
flipVariable v s 
  = let modifiedVarPos = IM.insert v changedVar (variablePosition s)
    in (s{numSATEDClauses=numSATEDClauses s + overAllChange,variablePosition=modifiedVarPos,clausePosition=modifiedClausePos},overAllChange)
  where
    overAllChange = ordChange + negChange
    changedVar  = not $ (variablePosition s) IM.! v
    (ords,negs) = (variableLookUp s) v
    cp = clausePosition s
    (cp',ordChange) = if changedVar then foldl countInc (cp,0) ords else foldl countDec (cp,0) ords
    (modifiedClausePos,negChange) = if changedVar then foldl countDec (cp',0) negs else foldl countInc (cp',0) negs
    countInc (positions,counter) i = let current =  positions IM.! i
                                         counter' = if current == 0 then counter+1 else counter
                                     in (IM.insert i (current+1) positions,counter')
    countDec (positions,counter) i = let current =  positions IM.! i
                                         counter' = if current == 1 then counter-1 else counter
                                     in (IM.insert i (current-1) positions,counter')


