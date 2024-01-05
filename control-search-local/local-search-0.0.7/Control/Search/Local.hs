{-| We capture the pattern of meta-heuristics and local search as a process or stream of 
    evolving solutions. These combinators provide a way to describe and manipulate these 
    processes quickly. The basic pattern of their use is this;

 > loopP (strategy) seed 

    The strategy itself is a stream transformer. The transformer becomes a search strategy 
    when it's output is fed back into it's input, which is the action of the loopP function.
    For example, the following is not a search strategy but you could write;

 > loopP (map (+1)) 0

    Which would generate the stream [0,1,2...
    A real search strategy then looks like;

  > loopP iterativeImprover tspSeed

    Many search strategies do not always produce improving sequences as the iterative improver does. For these
    we provide a simple modification of 'scanl' which can be applied to any stream, called 'bestSoFar'.
    Finally, these streams are usually descriptions of unlimited processes. To make them 
    practical we limit them using standard Haskell combinators such as 'take' and list index.

  > take 20 . bestSoFar $ loopP searchStrategy seed

    Search strategies are constructed via the composition of other functions. This often resembles the   
    composition of an arrow pipeline, and this library can be rewritten in terms of arrows, however we have 
    found no significant advantage in doing this. 
 
    A simple TABU like search strategy, that has a memory of the recent past (10 elements) of the search process, and 
    filters neighbourhoods accordingly can be created like this;
    
    > searchStrategy xs = map head $ zipWith (\ws->filter (flip notElem ws)) (window 10 xs) (neighbourhood xs)  

    A common way to improve meta-heuristics is to introduce stochastic elements, such as random decisions from a constrained
    set of choices, or neighbourhoods which will not generate exactly the same set of options each time a particular solution 
    is visited. Stream transformations allow this because they can thread additional state internally, while not exposing 
    the user of the transformation to a great deal of the process. For example in the above example, to create a random 
    choice from the constrained set at each point you would do this;

    > searchStrategy rs xs = select uniformCDF $ zipWith (\ws->filter (flip notElem ws)) (window 10 xs) (neighbourhood xs)
  
    The neighbourhood can be similarly modified. We must still provide the starting points for the extra data used by 
    such transformers, in this case a stream of random values, or in other cases a random number generator, but one provided
    it is hidden, and the transformer can be composed with any other transformation.

    Using the same transformation, which threads an internal state, in several places is harder. It involves 
    merging and dividing streams in sequenced patterns. For example;

    > applyToBoth tr as bs = (\xs->(map head xs,map last xs)) . chunk 2 $ tr (concat $ transpose [as,bs])
  -}

module Control.Search.Local(
  -- * Types
  StreamT,Stream,List,
  -- * Optimisable Type Class
  Optimisable,(=:=),(>:),(<:),best,worst,bestOf,sortO,bestSoFar,
  -- * Stream Transformation Combinators
  doMany,chunk,variableChunk,stretch,variableStretch,window,variableDoMany,varyWindow,improvement,
  select,select',until_,restart,restartExtract,nest,preNest,divide,join,
  -- * Loop Combinators
  loopP,loopS,
  -- * Helper functions
  safe,
  ) where

import Data.List
import System.Random
import System.IO.Unsafe

{-| In previous versions I have used the standard 'Eq' and 'Ord' classes. However I have then 
    had to assume that every problem is a minimisation. To get around this, and provide functions that 
    match more closely to optimisation problems, where the concept we seek is /better than/, rather than 
    /greater/ or /less/ than, I provide this new class. It is however very very like 'Ord'.

    It is proposed that this class is used for value of solution comparisons only, and the standard 'Eq' class
    is retained for situations where the solutions are identical, and do not just share the same value.-}
class Optimisable a where
  {-| Equal to, by solution quality.-}
  (=:=) :: a -> a -> Bool  
  {-| Better than, assumed to be by solution quality. -}
  (>:) :: a -> a -> Bool   
  {-| Worse than, assumed to be by solution quality. -}
  (<:) :: a -> a -> Bool   

{-| This returns the best solution in an input list. It can be thought of as similar to 'maximum'.-}
best :: Optimisable s => [s] -> s
best (x : xs) = foldl (\ a b -> if a >: b then a else b) x xs

{-| This returns the worse solution in an input list. It can be thought of as similar to 'minimum'.-}
worst :: Optimisable s => [s] -> s
worst (x : xs) = foldl (\ a b -> if a <: b then a else b) x xs

{-| This returns the best of two input solutions. It can be thought of as similar to 'max'.-}
bestOf :: Optimisable s => s -> s -> s
bestOf a b = if a >: b then a else b

{-| An alternative version of 'sort' (implemented in terms of 'sortBy'), 
    but using 'Optimisable', so that better solutions are earlier in the output list than worse ones.
-}
sortO :: Optimisable a => [a] -> [a]
sortO = sortBy (\ a b -> if a >: b then LT else GT)

{-| A transformer that is usually used as a final step in a process, to allow the user 
    to only see the best possible solution at each point, and ignore the intermediate values
    that a strategy may produce. 
-}
bestSoFar :: Optimisable s=>StreamT s s
bestSoFar (x:xs) = scanl bestOf x xs

{-| The basic stream transformation type. -}
type StreamT a b = Stream a->Stream b

{-| Internally streams are represented as standard Haskell lists. This type synonym is provided for readability. It is assumed that streams will be infinite.-}
type Stream s = [s]

{-| Lists are also used to represent finite collections or groups of solutions. This synonym is for where
    the list is being used as a finite list. -}
type List s = [s]

{-| The standard function for /tying the knot/ on a stream described process. 
    This links the outputs of the stream process to the inputs, with an initial set of values, and
    provides a single stream of values to the user. -}
loopS :: StreamT s s->StreamT s s
loopS streamT seed = let sols = seed ++ streamT sols in sols

{-| A more specific version of 'loopS' and implemented in terms of it. Rather than allowing a 
    number of initial values, this allows only 1.-}
loopP :: StreamT s s->s->Stream s
loopP f x = loopS f [x] 

{-| This was original created to assist with making multiple selections from a population within a genetic 
    algorithm. More generally this is a higher order function which will take a stream transformation and apply
    it multiple times to each element of the input stream gathering up the results. 

    It can be used to make multiple selections from a population in a GA, or to create a neighbourhood 
    function from a perturbation operation. For example;

    > nF = doMany 5 perturbFunction

    Where /5/ is just an arbitrary example constant and /perturbFunction/ is a placeholder.
     
    If the perturb function is defined with a parameter controlling the perturbation, for example a random number,  
    such as which pair of cities in a TSP to exchange then you can do this for a deterministic operation;

    > nF = doMany 5 (zipWith perturbationFunction (cycle [0..4]))

    For a more stochastic effect;
  
    > nF = doMany 5 (zipWith perturbationFunction (randoms g)) 

    Where /g/ is assumed to be of the 'System.Random.RandomGen' class.

 -}
doMany :: Int->StreamT s s'->StreamT s (List s')
doMany sz f = chunk sz . f . stretch sz

{-| /variableDoMany/ perfoms the same action as 'doMany', but allows the programmer to vary how many times 
    the transformation is applied to each underlying solution, through the use of a stream of sizes.-}
variableDoMany :: Stream Int->StreamT s s'->StreamT s (List s')
variableDoMany szs f = variableChunk szs . f . variableStretch szs

{-| Creates a rolling window over a stream of values up to the size parameter. The windows are then 
    produced as a stream of lists. This can also be done using a queue data structure, however this 
    was found to be slightly faster.-}
window :: Int->StreamT s (List s)
window sz xs = concat [zipWith take [0..sz] (repeat xs) ,map (take sz) . tails .drop (sz-1) $ xs]

{-| A function for transforming a stream of 'window's, taking random numbers of elements from the front 
    of each window. This can be used in the implementation of variations of the TABU search algorithm. 
    Usage as follows (with example values) ;

    > varyWindow (3,6) . window 10
-}
varyWindow :: (Int, Int) -> Stream (List s) -> Stream (List s)
varyWindow range = unsafePerformIO k
  where k = do g <- newStdGen
               return $ zipWith take (randomRs range g)

{-| Breaks down a stream into a series of chunks, frequently finds use in preparing sets of random numbers 
    for various functions, but also in the 'makePop' function that is important for genetic algorithms. -}
chunk :: Int->StreamT s (List s)
chunk i xs = let t = (take' i xs) in t `seq` (t : chunk i (drop i xs))
  where
    take' :: Int->[a]->[a]
    take' n _ | n<=0 = []
    take' n [] = []
    take' n (x:xs) = x `seq` (x : take' (n-1) xs)

--chunk i xs = (take' i xs) : chunk i (drop i xs)
-- chunk i xs = let (as,bs) = splitAt i xs in as : chunk i bs 
-- chunk i = unfoldr (Just . splitAt i) 

{-| A more flexible version of chunk which can vary the size of each chunk, in accordance with a list providing the 
    sizes required of each grouping. For example;

    > chunk i = variableChunk (repeat i)
    > variableChunk (cycle [1,2]) [0..] = [[0],[1,2],[3],[4,5]... 
-}
variableChunk :: Stream Int->StreamT s (List s)
variableChunk (i:is) xs = let t = take i xs in t : variableChunk is (drop i xs) 

{-| An operation which changes the structure of an underlying stream, but not the data type, by 
    replicating the elements of the stream in place. Can be thought of as changing the /speed/ of the stream. 
    This finds use in 'doMany', genetic algorithms and ant colony optimisation.
    For example;

    > stretch 2 "abcd" = "aabbccdd"

-}
stretch :: Int->StreamT s s
stretch sz = concatMap (replicate sz)

{-| A more flexible version of stretch which can vary how far each value in the underlying stream is stretched, 
    in accordance with a list providing the sizes required of each grouping. For example;

    > stretch i = variableStretch (repeat i)
    > variableStretch [3,3,7,2] "abcd" = "aaabbbcccccccdd" 
-}
variableStretch :: Stream Int->StreamT s s
variableStretch sz = concat . (zipWith replicate sz)

{-| This function transforms a neighbourhood function to give a transformation that yields /improving/ 
    neighbourhoods, that is, neighbourhoods that only contain solutions that improve upon the seed solution.
    In the case that there are no improving solutions (local minima), the output is a singleton list 
    containing the seed solution. This functionality is provided byt the 'safe' helper function.-}
improvement :: Optimisable s => StreamT s (List s) -> StreamT s (List s)
improvement nf sols = safe (map (:[]) sols)
                    $ zipWith (\a b -> filter (a >:) b) sols (nf sols)

{-| The generalised selection routine. This takes a stream of lists and selects one element from each list, 
    to construct the new stream. The selection routine takes a 'DistributionMaker', and selects based upon this.
    This only really makes sense when there is some internal structure in the stream of lists. For example;

    > select (poissonCDF 1) . map sortO   

    Each list in the input stream is sorted, so that the best solutions appear first. This is then selected from
    using a Poisson distribution, with the mean at 1. This means that the early (better) solutions are much more 
    likely to be selected.
-}
select :: (Random r, Ord r) => (Int -> List r) -> StreamT (List a) a
select mkDist = unsafePerformIO k 
  where k = do g <- newStdGen
               return $ select' (randoms g)
        select' rs xs = let dxs = map (\x -> zip (mkDist $ length x) x) xs
                       in zipWith s dxs rs
        s [x] _ = snd x
        s ((p, x) : xs) r = if r > p then s xs r else x

{-| This function acts like 'select', however the distribution is fixed, not being constructed anew for each 
    list in the input stream. This is much more efficient, but does assume that the size of each list in the 
    input is the same (a fixed size neighbourhood is perfect for this).
-}
select' :: (Random r,Ord r) => List r->StreamT (List a) a
select' dist = unsafePerformIO k 
  where k = do g <- newStdGen
               return $ select' (randoms g)
        select' rs xs = let dxs = map (\x -> zip dist x) xs
                       in zipWith s dxs rs
        s [x] _ = snd x
        s ((p, x) : xs) r = if r > p then s xs r else x

{-| This function is very similar to the /until/ function of FRP. It takes a stream and 
    returns the elements of that stream until 'True' appears on the trigger stream. At this point 
    one of the potential futures is chosen and becomes the remainder of the stream. 
    The potential futures are zipped with the triggers, so the choice is fixed, the /current/ potential 
    future is the choice. More generally this concept could be elaborated in the future.

    (1) The initial stream of values to place before the trigger
    (2) The stream of triggers
    (3) The stream of potential future streams

    Could be used to provide a temperature strategy that restarts once in Simulated Annealing like so;

    > let t = iterate (+1) 0 
    > in until_ t triggerStream (repeat t) 

    This alternative will restart every time 'True' appears on the trigger stream, not just the first time.

    > let t = until_ (iterate (+1) 0) triggerStream (repeat t) in t
-}
until_ :: Stream a -- stream of values, to place before trigger
       -> Stream Bool -- stream of triggers for switch over
       -> Stream (Stream a) -- stream of potential futures
       -> Stream a
until_ (a:_) (True:_) (_:(cs:_)) = a : cs
until_ (a : as) (False : bs) (_: cs) = a : until_ as bs cs

{-| A helper function that chooses between elements of the two input streams at each point in the 
    stream, and returns one which is non-empty. The check looks at values in the second stream first, 
    if this list is not empty, it is returned.
-}
safe :: Stream (List v) -> Stream (List v) -> Stream (List v)
safe = zipWith (\a b -> if null b then a else b)

{-| Restart is a little like loop. It will construct a stream of values by applying a stream transformation 
    to one value, and then the successor and so on. It differs in that it also takes a triggering mechanism
    that can choose to stop the current sequence and continue from a different value (the next value on the 
    initial stream). For example, the following will start counting initially from 0, then -5, then -10, and 
    will count until it reaches 11 each time. 

    > restart (map (+1)) (map (>10)) [0,-5,-10]
  -}
restart :: StreamT a a->StreamT a Bool->StreamT a a
restart f r (a : as) = let ms = loopP (nest [(False, id), (True, const as)] (r ms) . f ) a
                       in ms
{-| Exactly like 'restart', except that it will only return the result of an iteration of transformation,
    not the intermediate values. For example;

    > restartExtract (map (+1)) (map ((==0) . flip mod 4)) [1,-5,-10,-13]

    gives;

    > [4,-4,-8..

     -}
restartExtract :: StreamT a a->StreamT a Bool->StreamT a a
restartExtract f r (a : as) = let ms = loopP (nest [(False, id), (True, const as)] rs . f ) a
                                  rs = r ms
                              in [l | (p, l) <- zip (drop 2 $ window 2 rs) ms, p == [True, False]]

{-| Is the combination of 'divide' and 'join'. It takes a set of indices and stream transformations, 
    divides the input stream, using the indices and a stream of indices, transforms each substream 
    by the related stream transformation, and then puts them all back together again as a new stream.

    For example, to apply a transformation (f) to only every third value in a stream, you could do this;

    > nest [(True,id),(False,f)] (cycle [True,True,False])
-}
nest :: Eq n => List (n, Stream a -> Stream b) -> Stream n 
             -> Stream a -> Stream b
nest fs ns = flip join ns . zipWith (\(n, f ) s -> (n, f s)) fs 
           . divide (map fst fs) ns

{-| This function acts like 'nest', but the name stream makes choices (or can do) based upon the initial value
    of solutions on the input stream.-}
preNest :: Eq n => (Stream a -> Stream n) -> List (n, Stream a -> Stream b) 
                -> Stream a -> Stream b
preNest d fs xs = nest fs (d xs) xs

{-| Join is the inverse of 'divide', it combined substreams into a stream fo values. 
    It takes a list of substreams, and the indices that indicate them, and then a stream of the indices.
    For each value in the stream of indices, the /next/ value in the appropriate substream is chosen and  
    produced. 
-}
join :: Eq n => List (n, Stream v) -> Stream n -> Stream v
join streams ns = unfoldr f (ns, streams)
  where
    f (k : ks, vs) = let (as, (n, p : ps) : bs) = break ((== k) . fst) vs
                     in Just (p, (ks, as ++ (n, ps) : bs))

{-| Divide splits a stream of values into a list of substreams. The division is controlled by 
    a list of /indices/ and then a stream of these indices. -}
divide :: Eq n => List n -> Stream n -> Stream v -> List (Stream v)
divide indices ns xs = [substream (map (== n) ns) xs | n <- indices]
  where substream bs = map snd . filter fst . zip bs


