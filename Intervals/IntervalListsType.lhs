> module IntervalListsType where
> import Numeric.IEEE
> import Data.List
> import IntervalType
> import IntervalArithmetic

>{--
> Arithmetic operations on intervals mostly  yield intervals, the exception is  when a division by a
> zero containing interval is performed: then the result is a pair of intervals unless the divisor contains zero,
> too. This may also be the case in
> general functions applied to intervals. Resulting lists of intervals are interpreted as unions.
> In this module we extend arithmetics of intervals to arithmetics on lists of intervals.
> 
> Lists of intervals form a bounded lattice the bottom element is emptySet, the top element is the singleton set
> [(-inf,inf)] (fullSet)
> --}
>
> type ISet = [Interval]
>
> intToSet :: Interval -> ISet
> intToSet z = [z]
>
> emptySet :: ISet
> emptySet = []
>
> fullSet :: ISet
> fullSet =  intToSet $ num2Int(-infinity) infinity 
>

>
> 
> -- meet is the infimum  wrt. this order, since the empty interval is not an element of type Interval,
> -- we can only define it as an operation to lists of intervals
> 
> meetInt :: Interval -> Interval -> ISet
> meetInt z z' | (b >= a' && a <= b') = [num2Int (max a a')(min b b')]
>              | otherwise               = emptySet
>                where a = lb z
>                      b = rb z
>                      a' = lb z'
>                      b' = rb z' 
>
> -- hull is the join of two intervals; if the meet is nonempty and the smallest interval containing
> -- both else
> 
> hull :: Interval -> Interval -> Interval
> hull z z' = num2Int (min a a')(max b b')
>              where
>               a = lb z
>               b = rb z
>               a' = lb z'
>               b' = rb z'
>
> -- If we have as a result of some calculations a list of intervals we want to clean it, i.e.
> -- remove duplicates
> -- and melt together overlapping intervals to get a shorter list of disjoint intervals.
>
> cleanup :: [Interval] -> ISet
> cleanup [] = emptySet
> cleanup (z : zs) = (foldl hull z joints) :  disjoints
>                     where (joints, disjoints)  = partition (not.null.(meetInt z)) (cleanup zs)
>                     
>  -- join2 returns a clean set of intervals that covers the same Doubles as the input sets
> join2 :: ISet -> ISet -> ISet
> join2 zs zs' = cleanup $ zs ++ zs'
>
> union :: [ISet] -> ISet
> union = foldl join2 emptySet
>
> -- infimum on ISet
>

> intersect2 :: ISet -> ISet -> ISet
> intersect2 = liftToList2 meetInt
> 
> -- general intersection of sets
> 
> intersection :: [ISet] -> ISet
> intersection = foldl intersect2 fullSet
> 
> 
> -- liftToList takes a binary function f on Intervals and returns binary function on ISet, s.t. its application
> -- a clean list of pairwise results (to be abstracted once Interval is in type class lattice)
>
> liftToList :: (Interval -> Interval -> Interval) ->  ISet -> ISet -> ISet
> liftToList f zs zs' = cleanup [f z z' | z <- zs, z' <- zs']
>
> liftToList2 :: (Interval -> Interval -> ISet) ->  ISet -> ISet -> ISet
> liftToList2 f zs zs' = cleanup $ concat [f z z' | z <- zs, z' <- zs']

> -- arithmetic on interval lists
>
> addIL :: ISet -> ISet -> ISet
> addIL = liftToList add
>
> subIL :: ISet -> ISet -> ISet
> subIL = liftToList sub
>
> mulIL :: ISet -> ISet -> ISet
> mulIL = liftToList mul
>
> divIL :: ISet -> ISet -> ISet
> divIL = liftToList2 divI
>




