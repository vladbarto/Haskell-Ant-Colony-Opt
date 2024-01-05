{-|
  These combinators are for controlling local search processes at the top level and preventing stack and memory build ups.
  The basic combinators seen in the other libraries are all lazy and will describe the structure of the computations 
  that will make up the search. When it comes to accessing values and solutions from these processes you can print 
  each solution which will push the process forwards and avoid memory problems. 

  To avoid wasting processing time displaying many solutions in a process, when all you are interested in is the Nth one, 
  you might use the common list index function (!!). However this is a lazy operator and will cause Haskell to 
  construct the computation for the Nth value, in terms of the previous values, before beginning the evaluation. 
  This causes the memory problems. 

  Instead we provide an eager replacement for (!!) which we call (!!!). For more sophisticated applications we 
  provide two other semi-eager operations which return both an eager value and a lazy remainder.  
-}

module Control.Search.Local.Eager((!!!),indexWithRemainder,splitAt',push) where

import Data.List

{-| This is an eager list index. It acts exactly like the common (!!) operation, however it 
    evaluates each element to WHNF. In the case where each element of the list depends upon previous 
    elements in some way (usually true of the local search systems), this will result in the 
    computation being pushed forwards.  -}
(!!!) :: [a]->Int->a
(!!!) xs i = (push xs) !! i 

{-| Similar to the eager list index, however it also gives back the remainder of the computation as an
    unevaluated list. It is expected that this will be used to sample a stream for a human user, 
    allowing the user to see what has happened and make a decision to continue, or stop. If continue, then 
    the lazy remainder can be processed further. -}
indexWithRemainder :: [a]->Int->(a,[a])
indexWithRemainder xs i = (\(a:as)->(a,as)) . drop i . push $ xs

{-| Eager /splitAt/. Looks like /splitAt/, but the elements of the first list are evaluated to WHNF. -}
splitAt' :: Int->[a]->([a],[a])
splitAt' i = splitAt i . push 

{-| This is the helper function used by the others to force the list evaluation, as the list is accessed.
    I think this is equivalent to 
    
    > evalList rseq 

    from the /Control.Parallel.Strategies/ library, but reproduced here to reduce dependencies.
    -}
push :: [a] -> [a]
push (x : xs) = x `seq` x : push xs
