> module Intervals.IntervalArithmetic where
> import Intervals.IntervalType
> import Intervals.IntervalProp
> import Intervals.IntervalOps
> import Numeric.IEEE
> import Intervals.RoundedArithmetic
> import Data.List
> import Data.Ratio
> 
> --WARNING this module contains a declaration of Interval to an instance of class Fractional; this will
> -- only work if the divisors are strictly positive, ie. lb >0
>
> -- since NaN compares with every Double to False, max NaN x =NaN but max x NaN = x, that is a problem;
> -- to resolve it the function maxIEEE gives more intutive
> -- results as it ignores NaN
> 
> maxIEEE ::Double -> Double -> Double
> maxIEEE x y | isNaN x || isNaN y  = infinity
>             | otherwise           = max x y
> minIEEE ::Double -> Double -> Double
> minIEEE x y | isNaN x || isNaN y  = -infinity
>             | otherwise           = min x y

 implementation of arithmetic operations +,-,*,/ for intervals using the
 directed rounded operations add_u,etc.

> add :: Interval -> Interval -> Interval
> add x y = num2Interval (add_d (lb x)(lb y)) (add_u (rb x)(rb y))
>
> sub :: Interval -> Interval -> Interval
> sub x y = num2Interval (sub_d (lb x)(rb y)) (sub_u (rb x)(lb y))
>
> leftIntList :: Interval -> Interval -> [ Double]
> leftIntList x y =( mul_d (lb x)(lb y)): ((mul_d (lb x)(rb y)):
>                  ((mul_d (rb x)(lb y)):(( mul_d (rb x)(rb y)):[])))

> rightIntList :: Interval -> Interval -> [ Double]
> rightIntList x y =( mul_u (lb x)(lb y)): ((mul_u (lb x)(rb y)):
>                   ((mul_u (rb x)(lb y)):(( mul_u (rb x)(rb y)):[])))
>

------ nan cannot serve as the value of min/max on the empty list

> mul :: Interval -> Interval -> Interval
> mul x y = num2Interval (foldr (minIEEE) (infinity) (leftIntList x y))
>                      (foldr (maxIEEE)(-infinity) (rightIntList x y))
>
> -- division where divisor does not contain zeros, result is always a single interval,
> -- the exception of dividing by zero is handled by the control.exception module,
> -- divI and divI2 give as a result (-Infinity, Infinity) if either of the bounds of z or z' equals nan
> 
> divPlus :: Interval -> Interval -> Interval
> divPlus z z'| (c > 0) =  mul z (num2Interval (div_d 1 d) ( div_u 1 c))
>                          where c = lb z'
>                                d = rb z'
> 
> divI ::  Interval -> Interval -> [Interval]
> divI z z' 
>   | not (contZ z')
>        = [mul z (num2Interval (div_d 1 d) ( div_u 1 c))]
>   |contZ z && contZ z'
>        = [num2Interval (-infinity)(infinity)]
>   | isIntN z && isIntNZ z'
>        = [num2Interval (div_d b c) infinity]
>   | isIntN z && isIntM z'
>        = [(num2Interval (-infinity)(div_u b d)), (num2Interval (div_d b c) infinity)]
>   | isIntN z && isIntPZ z'
>        = [num2Interval (-infinity) (div_u b d)]
>   | isIntP z &&  isIntNZ z'
>        = [num2Interval (-infinity) (div_u a c)]
>   | isIntP z && isIntM z'
>        = [(num2Interval (-infinity)(div_u a c)), (num2Interval (div_d a d) infinity)]
>   | isIntP z && isIntPZ z'
>        = [num2Interval (div_d a d) (infinity)]
>   | otherwise = []
>   where a = lb z
>         b = rb z
>         c = lb z'
>         d = rb z'
>
> -- the multiplicative inverse of an interval, s.t. invInt(infInt z) = z unless isIntZ z(after clean up)
>
> invInt :: Interval -> [Interval]
> invInt z | isIntZ z  = []
>          | b == 0    = [num2Interval (-infinity) (div_u 1 a)]
>          | a == 0    = [num2Interval (div_d 1 b) infinity]
>          | isIntM z  = invInt (num2Interval a 0) ++ invInt (num2Interval 0 b)
>          | otherwise = [num2Interval (div_d 1 b) (div_u 1 a)]
>           where a = lb z
>                 b = rb z
> 
> -- interval division based on the inverse function, gives the same results as divI
> 
> divI2 ::  Interval -> Interval -> [Interval]
> divI2 z z' | contZ z' && contZ z  = [num2Interval (-infinity) infinity]
>            | otherwise            = map (mul z) (invInt z')
>
> testlist = [(num2Interval (-1) 1),(num2Interval 0 1), (num2Interval (-infinity)(-1)), (num2Interval (-1) infinity),
>             (num2Interval 0 0),(num2Interval (-3) (-1))]
>
> testdiv = [ divI2 (testlist !! i )(testlist !! j )| i <- [0..5],j<- [0..5]]
>
> testdivI = [ divI (testlist !! i )(testlist !! j )| i <- [0..5],j<- [0..5]]
> 
> sqrtI :: Interval -> Interval
> sqrtI z
>     = num2Interval (sqrt (lb z)) (sqrt (rb z))
>   
>
> instance Num Interval where
>  negate z    = num2Interval (- (rb z))(- (lb z))
>
>  z + z' = add z z'
> 
>  z * z' = mul z z'
> 
>  abs z
>   | isIntP z
>     = z
>   | isIntM z
>     = num2Interval 0 (len z)
>   | isIntN z
>     = negate z
>   | isIntZ z
>     = num2Interval 0 0
>   | otherwise
>     = num2Interval nan nan
>
>  signum z
>   | isIntP z
>     = num2Interval 1 1
>   | isIntN z
>     = num2Interval (-1) (-1)
>   | isIntZ z
>     = num2Interval 0 0
>   | isIntM z
>     = num2Interval ((lb z)/(len z))((rb z)/(len z)) 
>   | otherwise
>     = num2Interval nan nan
> 
>  fromInteger k = num2Interval (fromInteger k) (fromInteger k)

> instance Fractional Interval where
>  (/) = divPlus
>  fromRational r = double2Interval  (p / q)
>                   where p = fromInteger $ numerator r
>                         q = fromInteger $ denominator r
