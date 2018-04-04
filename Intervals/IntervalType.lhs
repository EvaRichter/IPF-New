> -- module IntervalType where
> module Intervals.IntervalType where
> import Numeric.IEEE
> import Data.List

 implements a type of set-based intervals, i.e. also unbounded
 intervals and empty set are allowed,

 (the empty interval will be represented by [nan,nan])


> data Interval = IV Double Double deriving (Eq,Read)
>
> instance Show Interval where
>      show (IV a b) = "["++(show a)++";" ++ (show b)++"]"
>
> lb :: Interval -> Double 
> lb (IV a b) = a
> 
> rb :: Interval -> Double
> rb (IV a b) = b
>
> givebs :: Interval -> (Double, Double)
> givebs (IV a b) = (a,b)
>
> num2Interval :: Double -> Double -> Interval
> num2Interval a b = IV a b
>
> double2Interval :: Double -> Interval
> double2Interval x = num2Interval x x
>



 
 


 

 
 


  



