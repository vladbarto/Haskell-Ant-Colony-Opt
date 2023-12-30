{-# LANGUAGE RecordWildCards #-}
module ProbableAlgorithms.Colony.Solver (runSolver) where

import Problems.NPHARD.TSP
import ProbableAlgorithms.Colony.Ants

import Prelude hiding (mapM_,foldr,concat)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (foldrM, mapM_,foldr,concat)
import Data.Maybe
import Control.Lens

import Control.Applicative
import Control.Monad hiding (mapM_)
--import Control.Monad.Trans.Iter

import Control.Monad.Random
import Control.Monad.State.Strict hiding (mapM_)
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Writer.Strict hiding (mapM_)
import Control.Monad.Trans.Maybe

type Problem = Country
type SolutionState = AntColony

type SolverT a = MaybeT
                    (WriterT Leaderboard
                        (ReaderT (Problem,AntConfig)
                            (StateT SolutionState
                                (Rand StdGen)))) a

embed :: Maybe a -> SolverT a
embed = MaybeT . return


{-| Pick a city at random, return it along with a new antrng. The random city is used to determine
    where a DeadAnt will respawn
-}
randomCity :: SolverT City
randomCity = do
    cits <- asks $ getCities . fst
    x <- getRandomR (0,M.size cits -1)
    embed $ M.lookup x cits


-- | Retrieve scent map details for an edge (edge must exist or this will fail)
scentMapLookup :: Edge -> SolverT ScentPoints
scentMapLookup e = do
    m <- use colmap
    embed . M.lookup e $ getMap m

newPath :: City -> Path
newPath = Path [] . (:[])

newCityAnt :: Ant -> City -> SolverT CityAnt
newCityAnt a c = do
    cfg <- asks snd
    return $ CityAnt $ MainAnt a (newPath c) (scentScore cfg)

   

{-| Each round the dead are "revived" - whereby their "respawn time" is decremented or if it has
    reached 0 then a new CityAnt is created to replace the DeadAnt and placed randomly in a city.
-}
reviveDead :: SolverT ()
reviveDead = do
        oldcants <- use cants
        olddants <- use dants
        (nca,nda) <- foldrM foldfunc (oldcants,S.empty) (S.toAscList olddants)
        cants .= nca
        dants .= nda
        return ()
    where
        foldfunc :: DeadAnt -> (S.Set CityAnt,S.Set DeadAnt) -> SolverT (S.Set CityAnt,S.Set DeadAnt)
        foldfunc d (sca,sda)
            | spawnTime d == 0 = do
                nc <- randomCity
                sca' <- S.insert <$> newCityAnt (deadAnt d) nc <*> return sca
                return (sca',sda)
            | otherwise = let sda' = DeadAnt (deadAnt d) (spawnTime d - 1) `S.insert` sda
                          in  return (sca,sda')


{-| The exploreEdges function is the most complex component of the algorithm because it needs to
    factor in a number of issues:
        1. It must not allow the ant to go back over the same edge it came from as this creates
        an incredibly pointless ant death that has severe performance impacts.
        2. It must get all of the remaining edges for the particular CityAnt
        3. For each edge choice it must fetch the "scent score" for that edge from the scent map
        4. It then needs to present a set of "loaded" choices - it does this by replicating each
        choice by the amount of its scent score.
        5. Finally a random choice must be made for each loaded choice while simultaneously
        the antrng so that the next ant has their own random choice.
-}
exploreEdges :: SolverT ()
exploreEdges = do
        neweas <- use cants 
                    >>= 
                        \x -> use eants >>= 
                            \y -> foldrM (\a acc -> flip S.insert acc <$> makeEdgeAnt a) y x
        eants .= neweas
        cants .= S.empty
        return ()
    where
        replicaFromScent :: Edge -> SolverT [Edge]
        replicaFromScent er = flip replicate er <$> scentMapLookup er
        currentCity :: CityAnt -> City
        currentCity = head . cityPath . getPath . cityAnt
        makeEdgeAnt :: CityAnt -> SolverT EdgeAnt
        makeEdgeAnt ca@CityAnt{..} = do
            (ne,ixs) <- makeChoice ca
            cits <- asks $ getCities . fst
            cit <- embed $ M.lookup ixs cits
            cfg <- asks snd
            let oldPath = getPath cityAnt
                newPathExplore = Path (ne : edgePath oldPath) (cit : cityPath oldPath)
                newScent = getScent cityAnt + distanceBonus cfg
                newMainAnt = MainAnt (getAnt cityAnt) newPathExplore newScent
            return $ EdgeAnt newMainAnt ne cit (edgeWeight ne)
        makeChoice :: CityAnt -> SolverT (Edge,Id)
        makeChoice ca = (cityChoices ca <$> edgeChoices ca) >>= uniform
        cityChoices :: CityAnt -> [Edge] -> [(Edge,Id)]
        cityChoices CityAnt{..} = map mapFunc
            where
                mapFunc :: Edge -> (Edge,Id)
                mapFunc Edge{..} = if cityLeft == getId (currentCity CityAnt{..}) then (Edge{..},cityRight) else (Edge{..},cityLeft)
        edgeChoices :: CityAnt -> SolverT [Edge]
        edgeChoices CityAnt{..} = concat <$> foldrM foldFunc [] (edges (currentCity CityAnt{..}))
            where
                foldFunc :: Edge -> [[Edge]] -> SolverT [[Edge]]
                foldFunc x accum
                    | not (null . edgePath . getPath $ cityAnt) && (x == head (edgePath . getPath $ cityAnt)) = return accum
                    | otherwise = (: accum) <$> replicaFromScent x


{-| Advance each EdgeAnt based on the following conditions:
        1. Reduce its wait time by 1 if wait time > 0. Otherwise:
        2. Check to see if the new destination means it has looped back on itself:
            2A. Create a DeadAnt
            2B. If the loop is a Hamiltonian Cycle create a Champion to represent its triumph AND
            also drop scent points on the edge (where as a regular death drops no points)
            2C. If the loop is not Hamiltonian then check to see if it is the longest cycle
            discovered to-date. If it is check if it has the lowest weight. If it does, this
            becomes a temporary leader and is specified as the Maybe EdgeAnt in the colony.
        3. Otherwise make a CityAnt
            3A. Drop scent points on the edge that was traversed
            3B. Increment the amount of scent points it will drop next time by the distance bonus

    Additionally, during the Advance Edges stage the Leaderboard is updated for any ants that
    complete either a loop or a Hamiltonian Cycle. This is done using the mappend function 
    for the leaderboards Monid instance.
-}
advanceEdges :: SolverT ()
advanceEdges = use eants >>= mapM_ foldfunc
    where
        foldfunc :: EdgeAnt -> SolverT ()
        foldfunc EdgeAnt{..}
            | waitTime > 1 = do
                let ea = EdgeAnt { waitTime = waitTime - 1, ..}
                eants %= S.insert ea
            | otherwise = checkForProgress EdgeAnt{..}
        checkForProgress :: EdgeAnt -> SolverT ()
        checkForProgress ea@EdgeAnt{..} = asks fst >>= extension . extendPath (getPath edgeAnt)
            where
                extension :: ExtendPath -> SolverT ()
                extension BrokePath{} = error "A broken path? Well this should never happen"
                extension IncPath{} = do
                    cants %= S.insert (CityAnt edgeAnt)
                    eants %= S.delete ea
                    return ()
                extension (LoopPath Path{..}) = do
                    tell . SLeaderboard . flip (SubLeaderboard (length edgePath)) Path{..} $ foldr ((+) . edgeWeight) 0 edgePath
                    eants %= S.delete ea
                    dsp <- asks (deathSpawnTime . snd)
                    dants %= S.insert (DeadAnt (getAnt edgeAnt) dsp)
                    return ()
                extension (Cycle Path{..}) = do
                    let hcyc = hcycleFromPath Path{..}
                        cycscore = weightscore hcyc
                    tell . MLeaderboard $ MainLeaderboard (M.singleton cycscore [hcyc]) (S.singleton hcyc)
                    eants %= S.delete ea
                    dants %= S.insert (DeadAnt (getAnt edgeAnt) 0)
                    return ()


-- | This simply applies the pre-defined decay function to every edge in the scent map
cleanScents :: SolverT ()
cleanScents = do
    df <- asks (decayFunction . snd)
    bs <- asks (edgeBaseScent . snd)
    let perEdge x = let x' = df x in if x' <= bs then bs else x'
    sm <- use colmap
    colmap .= ScentMap (M.map perEdge $ getMap sm)
    return ()


-- TODO: Try refactor using IterT
-- solverRound :: Iter (SolverT ())
-- solverRound = return $ reviveDead >> exploreEdges >> advanceEdges >> cleanScents

-- solverRounds :: Int -> SolverT ()
-- solverRounds = join . embed . runIdentity . retract . flip cutoff solverRound . fromIntegral

-- | Run multiple rounds of the solver based off of some cutoff value
solverRounds :: Int -> SolverT ()
solverRounds = flip replicateM_ (reviveDead >> exploreEdges >> advanceEdges >> cleanScents)


-- | Return a string output of the pertinent information, as well as the leaderboard
finaliseSolver :: StdGen -> Problem -> AntConfig -> SolverT () -> (String,Leaderboard)
finaliseSolver gen p ac st = (openingStr ++ "\n" ++ show col ++ "\n-----\n\n" ++ show l,l)
    where
        ((mr,l),col) = flip evalRand gen . flip runStateT (blankColony ac p) . flip runReaderT (p,ac) . runWriterT $ runMaybeT st
        openingStr = if isNothing mr
                            then "There was an error running the solver: Unexpected Maybe (likely from Map Loookup). State prior to error:\n"
                            else "Completed Ant Colony Algorithm:\n"

-- | Returns a Leaderboard along with a string representation of the outcome of the solver
runSolver :: StdGen -> Problem -> AntConfig -> (String,Leaderboard)
runSolver gen co ac = finaliseSolver gen co ac . solverRounds $ noOfRounds ac