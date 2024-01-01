{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ProbableAlgorithms.Colony.Ants where

import Problems.NPHARD.TSP

import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid

-- | Represents a complete hamiltonian cycle. No guarantees are made that this in fact is a hamiltonian cycle, it is merely a convenient data structure.
data Hcycle = Hcycle { getCycle::[(City,Edge)],citySet::S.Set City,edgeSet::S.Set Edge,weightscore::Int }

instance Show Hcycle where
    show x = "Cycle w/ score: " ++ show (weightscore x) ++ " and path: " ++ show (map snd (getCycle x))
instance Eq Hcycle where
    x == y = citySet x == citySet y
instance Ord Hcycle where
    compare x y = compare (citySet x) (citySet y)


-- | Generate a Hamiltonian Cycle from a path (this function simply assumes that the end product is
--   in fact a cycle. The checking should be done elsewhere.
hcycleFromPath :: Path -> Hcycle
hcycleFromPath p = Hcycle cyclepair cityset edgeset score
    where
        cyclepair = zip (cityPath p) (edgePath p)
        cityset = S.fromList (cityPath p)
        edgeset = S.fromList (edgePath p)
        score = S.fold (\x accum -> accum + edgeWeight x) 0 edgeset


-- | A useful utility to prepare a path for pattern matching
data ExtendPath = Cycle Path | IncPath Path | LoopPath Path | BrokePath Path deriving (Show)

-- | Extends a basic path into one of 4 constructors, used to identify a hamiltonian cycle
extendPath :: Path -> Country -> ExtendPath
extendPath p c
    | isBroken = BrokePath p
    | isCycle = Cycle p
    | isLoop = LoopPath p
    | otherwise = IncPath p
    where
        lastEdge = head (edgePath p)
        lastCity = head (cityPath p)
        earlyCities = tail (cityPath p)
        last2City = head earlyCities
        isBroken = lastEdge `notElem` edges last2City
        isLoop = lastCity `elem` earlyCities
        isCycle = (lastCity == head (cityPath p)) && (M.size (getCities c) == length earlyCities)


-- | Rounds (current,max)
type Round = (Int,Int)



{-| The "Main" leaderboard is essentially the high score table for an individual execution 
    of the ant colony algorithm. The key is the total cost of the cycle and the value 
    is a list of all paths that have that score (i.e. there may be multiple paths with the
    same score). The Show instance for Leaderboard presents it in the form of a high score table.
-}
data MainLeaderboard = MainLeaderboard { getTable::M.Map Int [Hcycle],getUnique::S.Set Hcycle }

instance Show MainLeaderboard where
    show x = "Leaderboard: " ++ (fst . foldl foldFunc ("",1) . M.toAscList $ getTable x)
        where
            foldFunc :: (String,Int) -> (Int,[Hcycle]) -> (String,Int)
            foldFunc (a,b) acc = (a ++ "\r\n>> " ++ show b ++ ". " ++ show acc,b+1)

instance Semigroup MainLeaderboard where
    (<>) = mappend

instance Semigroup Leaderboard where
    (<>) = mappend

instance Monoid MainLeaderboard where
    mempty = MainLeaderboard M.empty S.empty
    mappend (MainLeaderboard t1 u1) (MainLeaderboard t2 u2) = MainLeaderboard (M.unionWith (++) t2 t1) (S.union u1 u2)

{-| A second "Sub" leaderboard keeps track of the longest cycle that is not hamiltonian. Where
    two paths tie for length the one with the lowest weight is selected.
-}
data SubLeaderboard = SubLeaderboard { lpLength::Int, lpScore::Int, longestPath::Path }

instance Show SubLeaderboard where
    show x =    "No Hamiltonian Cycles found. Longest cycle was " 
             ++ show (lpLength x) ++ ", with score "
             ++ show (lpScore x) ++ ". Path details:\r\n" 
             ++ show (longestPath x)



{-| The Leaderboard can ultimately be either a "Main" or "Sub" leaderboard depending on whether
    or not any hamiltonian paths were found.
    
    Ultimately this has been implemented very much after the fact.
-}
data Leaderboard = MLeaderboard MainLeaderboard | SLeaderboard SubLeaderboard
-- TODO: Refactor AntSolver to properly accommodate 2 types of leaderboards

instance Show Leaderboard where
    show (MLeaderboard l) = show l
    show (SLeaderboard l) = show l

instance Monoid Leaderboard where
    mempty = SLeaderboard . SubLeaderboard 0 0 $ Path [] []
    mappend (MLeaderboard a) (MLeaderboard b) = MLeaderboard $ mappend a b
    mappend ml@MLeaderboard{} _ = ml
    mappend _ ml@MLeaderboard{} = ml
    mappend sla@(SLeaderboard la) slb@(SLeaderboard lb)
        | lpLength la > lpLength lb = sla
        | lpLength la == lpLength lb = if lpScore lb < lpScore la then slb else sla
        | otherwise = slb

{-| Each Ant is represented by a unique id. This is not rigidly enforced but is an
    active property of the algorithm.
-}
type Ant = Int

{-
    TYPES OF ANTS ***************

    1.  City Ant: The City Ant is currently located in a city. These ants will all move to become
        the next type: Edge Ants.
    2.  Edge Ant: Currently located on an edge between cities. There are 3 options for these
        ants. (a) They will stay on the edge for another round, (b) they will die and become
        a Dead Ant, or (b) They will arrive at their destination and become a City Ant.
    3.  Dead Ant: Ants that have done something to deserve algorithmic death. They will be revived
        after their respawn time at which point they become a City Ant. An ant dies if it reaches
        a city from an edge but it is a city which it has already been to - meaning it has done a
        loop. If the loop is actually a Hamiltonian Cycle a Champion is also created.
-}

{-| The MainAnt acts as an abstraction for the core component of a CityAnt or EdgeAnt:
        > The Ant ID#
        > The Path the ant has taken so far
        > The number of scent points the ant has accumulated so far. Each extra city the ant
        visits endows it with additional scent points, thereby favouring longer paths.
-}
data MainAnt = MainAnt { getAnt::Ant,getPath::Path,getScent::ScentPoints} deriving (Show)

-- | CityAnt: Currently resides in a city
data CityAnt = CityAnt { cityAnt::MainAnt } deriving (Show)
instance Eq CityAnt where
    x == y = (getAnt $ cityAnt x) == (getAnt $ cityAnt y)
instance Ord CityAnt where
    compare x y = compare (getAnt $ cityAnt x) (getAnt $ cityAnt y)

-- | EdgeAnt: Currently reside on an Edge
data EdgeAnt = EdgeAnt { edgeAnt::MainAnt,location::Edge,destination::City,waitTime::Int } deriving (Show)
instance Eq EdgeAnt where
    x == y = getAnt (edgeAnt x) == getAnt (edgeAnt y)
instance Ord EdgeAnt where
    compare x y = getAnt (edgeAnt x) `compare` getAnt (edgeAnt y)
    
-- | DeadAnt: Currently in ant heaven, waiting for reincarnation
data DeadAnt = DeadAnt { deadAnt::Ant,spawnTime::Int } deriving (Show)
instance Eq DeadAnt where
    x == y = deadAnt x == deadAnt y
instance Ord DeadAnt where
    compare x y = deadAnt x `compare` deadAnt y


{-| The ant colony has 4 SETS of ants, which are representative of each ant type. Why a set instead
    of a list? This is simply more representative of what they truly represent compared to a list.
    Each ant set should be unique. Further - no ant should be in more than one set (except for the
    Champion set which is just the cycle a champion ant took).
-}
data AntColony = AntColony 
    { _cants::S.Set CityAnt
    , _eants::S.Set EdgeAnt
    , _dants::S.Set DeadAnt
    , _colmap::ScentMap }
instance Show AntColony where
    show AntColony{..} =     "Ant Colony: " 
                         ++ (show $ S.size _cants) ++ " CityAnts, " 
                         ++ (show $ S.size _eants) ++ " EdgeAnts, "
                         ++ (show $ S.size _dants) ++ " DeadAnts"
                         ++ "\n-----\nScent Map: \n"
                         ++ show _colmap

blankColony :: AntConfig -> Country -> AntColony
blankColony cfg co = AntColony S.empty S.empty startingAnts newScentMap
    where
        startingAnts = S.fromList $ map (flip DeadAnt 1) [1,2..noOfAnts cfg]
        newScentMap = ScentMap . M.fromList . map (\x -> (x,edgeBaseScent cfg)) . S.toAscList $ getEdges co

{-| The crux of an Ant Colony Optimisation Algorithm is the scent trail left by an ant. This type
    alias represents a useful way to represent scent points throughout the rest of the algorithm.
-}
type ScentPoints = Int

{-| Each edge will have a corresponding scent point value. If an edge has a higher scent score that
    means it has been more highly travelled. The algorithm has a number of steps in place to ensure
    two things:
        > Edges that ultimately lead to (non-cycle) loops are less likely to be travelled
        > The lowest weighted edge that can form a cycle is more favoured
-}
newtype ScentMap = ScentMap { getMap::M.Map Edge ScentPoints } deriving (Show)

{-| Certain values in the algorithm are constant. As a result it is possible (in fact required) to
    define these before the algorithm commences. The values are as follows:
        1.  noOfAnts:: The more ants there are in the algorithm the less randomness plays a role
            in the final outcome. In general the more ants you have the more likely you are to
            find any cycle at all and the better the end result will be. There may be some
            theoretical limit at which more ants becomes counter productive but none has so far
            been observed. The more ants the slower the algorithm performs. For smaller graphs
            There is going to be a point at which it is more efficient to simply brute force
            every possible path non-deterministically rather than increase the number of ants.
        2.  noOfRounds: The longer the algorithm runs for the less randomness plays a role in the
            final outcome. In theory if the algorithm ran for infinite time it would find the
            absolute answer to a TSP-OPTIMIZE question. As TSP-OPTIMIZE is an NP-HARD problem it
            is not even possible to verify the answer in polynomial time. Choosing how many rounds
            to run for. Like the ant count, increasing the round count also becomes pointless if
            an actual answer could have been achieved in the same time through brute force.
        3.  edgeBaseScent:: Ants choose which edge to go down via a random dice throw - but one
            in which the dice is loaded. In this case, an Ant is more likely to choose an edge if
            it already has a scent trail deposited on it. The "base scent" is the starting scent
            value for every edge. The higher this is, the more scents have to be dropped on to
            an egde by an ant to sway other ants choices.
        4.  distanceBonus:: In order to encourage ants to traverse a longer (i.e. "more complete"
            path) it is possible to add a bonus modifier - this increases the value of a scent
            drop for every city that the ant has visited.
        5.  scentScore:: This is the default number of scent points an ant drops if it successfully
            reaches a city (i.e. it arrives at a city it has not visited before). This score is
            independent of any distance bonus.
        6.  deathSpawnTime:: Ants actually have to "wait" to traverse an edge. Their wait time is
            linked to the weight of the edge. In this way Ants are more likely to drop a scent
            score for following a "quicker" path. In an earlier iteration of the algorithm ants
            respawned automatically and this caused a flaw - ants were much less likely to find
            a Hamiltonian Cycle. This is because "death" had a lower cost than traversing a slow
            but ultimaely necessary edge. To compensate for this ants now have a respawn delay.
            Intuitively this should be AT LEAST as long as the weight of the largest edge.
        7.  decayFuction:: because edges are chosen at random and it may take some time for an
            ant to loop back on itself and die, it is necessary to "decay" a scent trail over time.
            The decay function represents how many scent points to remove from each edge each
            round. The simplest is to use (s `div` 2).
-}
data AntConfig = AntConfig 
    { noOfAnts::Int
    , noOfRounds::Int
    , edgeBaseScent::Int
    , distanceBonus::Int
    , scentScore::Int
    , deathSpawnTime::Int
    , decayFunction:: Int -> Int
    }
instance Show AntConfig where
    -- Unfortunately there is no easy way to do a Show for the decay function
    show AntConfig{..} = 
                "Ant Configuration: "
             ++ show noOfAnts ++ show noOfRounds
             ++ show edgeBaseScent ++ show distanceBonus
             ++ show scentScore ++ show deathSpawnTime


makeLenses ''AntColony



