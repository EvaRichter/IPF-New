> module OldStuffInterval where
> import Numeric.IEEE
> import Data.List
> import IntervalType
> import IntervalArithmetic

> --  It aggregates intervals with nonempty
> --  meet, and gives the first interval in case of disjoint intervals.


> melt :: Interval -> Interval -> Interval
> melt z z' | (meetInt z z' == []) = z
>           | otherwise         =  num2Int (min a a')(max b b')
>              where
>               a = lb z
>               b = rb z
>               a' = lb z'
>               b' = rb z'  
>

cleanList creates a list of disjoint intervals from a list of intervals(and removes duplicates)

> cleanList :: [Interval] -> [Interval]
> cleanList [] = []
> cleanList (z : zs) = (foldl hull z zsdm) : zsdn
>                     where
>                      zsd = cleanList zs 
>                      (zsdm, zsdn)  = partition (not.null.(meetInt z)) zsd

> meet1 :: Interval -> [Interval] -> [Interval]
> meet1  z zs     = concat (map (meetInt z) zs)
>
> 
>
> meet :: [Interval] -> [Interval] -> [Interval]
> meet  [] zs'         = []
> meet  (z : zs) zs'   = (meet1 z zs') ++ (meet zs zs')

> intersect2' :: ISet -> ISet -> ISet
> intersect2' zs zs' = concat $ map (intersectInt zs) zs'
>                      where intersectInt :: ISet -> Interval -> ISet
>                            intersectInt zs z' = concat $ map (meetInt z') zs
>
