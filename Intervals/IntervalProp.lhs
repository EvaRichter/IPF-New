> module Intervals.IntervalProp where
> --module IntervalProp where
> import Numeric.IEEE
> import Data.List
> --import IntervalType
> import Intervals.IntervalType

> -- implements properties for the interval type

> -- check whether an interval is empty, i.e. [nan,nan]
> -- an interval is valid if lb <= rb

> isEmpty :: Interval -> Bool
> isEmpty z = isNaN a || isNaN b || a == infinity ||
>             b == (-infinity) || (a > b) 
>             where
>              a = lb z
>              b = rb z


> validInt :: Interval -> Bool
> validInt z = a <= b
>              where
>               a = lb z
>               b = rb z
>  

 
 Helper functions to classify intervals. We have 6 distinct classes of valid intervals: mixed, zero-only, positive, positive with lb =0
 negative, negative with rb = 0, returns False for Intervals that are not valid

> isIntM :: Interval -> Bool
> isIntM z = (a < 0) && (b > 0)
>            where
>             a = lb z
>             b = rb z
>
> isIntZ :: Interval -> Bool
> isIntZ z = (a == 0) && (b == 0)
>            where
>             a = lb z
>             b = rb z


> isIntP :: Interval -> Bool
> isIntP z = (validInt z) && (a > 0)
>             where
>              a = lb z
> isIntPZ :: Interval -> Bool
> isIntPZ z = (a == 0) && (b > 0)
>             where
>              a = lb z
>              b = rb z
>    
>
> isIntN :: Interval -> Bool
> isIntN z = (validInt z) && (b < 0)
>              where
>               b = rb z
>
> isIntNZ :: Interval -> Bool
> isIntNZ z = (a < 0) && (b == 0)
>              where
>               a = lb z
>               b = rb z
  
  
> -- contZ is true if the interval contains zero, is False for a = NaN or b = NaN
> 
> contZ :: Interval -> Bool
> contZ z  = (a <= 0) && (b >= 0)
>                   where
>                    a = lb z
>                    b = rb z
>              
