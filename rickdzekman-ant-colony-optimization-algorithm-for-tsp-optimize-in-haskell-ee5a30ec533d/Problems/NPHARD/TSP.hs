module Problems.NPHARD.TSP where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

-- A useful representation for the map of cities
data Country = Country { getCities::M.Map Id City,getEdges::S.Set Edge } deriving (Show)

-- Useful for knowing this refers to the index of a city as key in a map
-- Used for extracting a keylist of cities
type Id = Int

-- Define a type for the problem instance
type Coord = (Double, Double)
type Problem = [Coord]

-- A city contains its ID (its index in a Map) and a list of Edges
data City = City { getId::Id,edges::[Edge] }
instance Eq City where
    x == y = getId x == getId y
instance Ord City where
    compare x y = getId x `compare` getId  y
instance Show City where
    show x = "City(id:" ++ (show $ getId x) ++ ") w/ Edges: (" ++ (show $ edges x) ++ ")"

-- Edges contain links between two cities. It should be the case that the left city always
-- has the smaller id
data Edge = Edge { cityLeft::Id,cityRight::Id,edgeWeight::Int }
instance Eq Edge where
    x == y = cityLeft x == cityLeft y && cityRight x == cityRight y
instance Ord Edge where
    compare x y = if a then b else (cityLeft x) `compare` (cityLeft y)
        where
            a = (cityLeft x) == (cityLeft y)
            b = (cityRight x) `compare` (cityRight y)
instance Show Edge where
    show x = "Edge (" ++ (show $ edgeWeight x) ++ "): " ++ (show $ cityLeft x) ++ "--" ++ (show $ cityRight x)

-- TODO: Create a graphViz export for paths
data Path = Path { edgePath::[Edge],cityPath::[City] }
instance Show Path where
    show x = start ++ (rec (edgePath x) (cityPath x) "") ++ end
        where
            rec :: [Edge] -> [City] -> String -> String
            rec [] (c:[]) accum = accum ++ "end: C" ++ (show $ getId c)
            rec [] _ accum = accum
            rec _ (_:[]) _ = error "path must end with a city"
            rec (e:es) (c1:c2:cs) accum = rec es (c2:cs) accum'
                where accum' = accum ++ "C" ++ (show $ getId c1) ++ " -> (" ++ (show $ edgeWeight e) ++ ") -> " ++ "C" ++ (show $ getId c2) ++ "\r\n"
            start = if (null $ cityPath x) then "Path {" else "Path {\r\nStart: C" ++ (show $ getId $ head $ cityPath x) ++ "\r\n"
            end = "\r\n}"


-- Connect two cities in a map of cities, organised by ID
connectEdges :: M.Map Id City -> Id -> Id -> Int -> M.Map Id City
connectEdges c c1' c2' weight = if c1' > c2' then connectEdges' c2' c1' else connectEdges' c1' c2'
    where
        connectEdges' c1 c2
            | nedge `elem` (edges getC1) = c
            | otherwise = let (c1u,c2u) = (updateCity getC1,updateCity getC2) in M.insert c1 c1u . M.insert c2 c2u $ c
            where
                -- Create a new edge from city #1 to city #2, using the weight provided
                nedge = Edge c1 c2 weight
                -- Get C1 from Just. There is no Nothing pipe, which probably shouldn't just
                -- be assumed. This is fine so long as the code leading here is correct.
                getC1 :: City
                getC1 = fromJust $ M.lookup c1 c
                -- As above with city #1
                getC2 :: City
                getC2 = fromJust $ M.lookup c2 c
                -- Update a city with a new edge
                updateCity :: City -> City
                updateCity x = City (getId x) (nedge:(edges x))

-- Return a list of cities which exculeds one
excludeOne :: M.Map Id City -> City -> M.Map Id City
excludeOne country excluder = M.delete (getId excluder) country

-- create a set of edges from a Map
edgesetFromMap :: M.Map Id City -> S.Set Edge
edgesetFromMap cm = M.foldl (\accum c -> foldl (flip S.insert) accum (edges c)) S.empty cm

-- Formats a city into a dot format string that can be parsed by GraphViz
generateGraphviz :: Country -> String
generateGraphviz country = foldToGraph ++ close
    where
        foldToGraph = S.fold (drawEdge) open (getEdges country)
        drawEdge e accum = accum ++ " C" ++ (show $ cityLeft e) ++ " -- " ++ "C" ++ (show $ cityRight e) ++ " [label=" ++ (show $ edgeWeight e) ++ "];\r\n"
        open = "graph tsp { \r\n"
        close = "\r\n }"
        

-- Scris ulterior        
-- Assuming you have a function like this in Problems.NPHARD.TSP module
createCountry :: Problem -> Country
createCountry coords = Country cities edges
  where
    cities = M.fromList $ zip [1..] $ map createCity coords
    edges = edgesetFromMap cities

    createCity :: Coord -> City
    createCity (x, y) = City cityId []
      where
        cityId = length cities + 1

