
> module Main where
>
> import Data.Array
> import Control.Monad
> import System.Environment
> import Numeric.IEEE
> import System.IO
> import IPFInputOutput
> import Examples
> import GHC.Float
> import Data.List


> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalOps
> import Intervals.IntervalProp
> 
> 
> numIt = 10 -- number of iterations
> numTestMx = 10
> numTestMg = 10
> dimensions = [(2,2),(2,5),(2,10),(5,5),(5,10),(10,10)]
>

> -- combines inputs from all files with fitting dimensions
> testAll :: IO()
> testAll = foldl (>>) (return()) [readWriteIPF dim indMx indMg |
>                      dim   <- dimensions,
>                      indMx <- [1.. numTestMx],
>                      indMg <- [1.. numTestMg]
>                             ]

> dummytest :: IO()
> dummytest =  do s <- readFile "IPFTestInput/2by2/marginals/1.txt"
>                 writeFile "dummy.txt" s
>                 let x = readDoubleMarginals s
>                 putStrLn (show x)
>                 return()
>
> machArray :: (Array (Int,Int) Double)
> machArray = array ((1,1),(2,1)) [((1,1), 40),((2,1), 50)]
>
> dummytest2 :: IO()
> dummytest2 =  do s <- readFile "dummytest3.txt"
>                  writeFile "dummy2.txt" s
>                  let x = readDoubleMatrix s
>                  putStrLn (show x)
>                  return()
> 
> {- this stuff (and may other functions in this file) should probably go into separate files but we gotta find a good structure -}
>
> {- takes a sample matrix (with marginals, which are then dropped) and outputs all pairs of 
>    average interval values with interval size to be displayed in a nice graphic -}
>
> ivLengthByNumberSize :: (Array (Int,Int) Interval) -> [(Double, Double)]
> ivLengthByNumberSize mx = [sizeAndLength $ mx'!(i,j) | (i,j) <- range $ bounds mx']
>                           where mx'          = innerArr mx
>                                 sizeAndLength i = (cen i, len i)          
> 
>
> ivBoundDeviations :: (Array (Int, Int) Interval) -> [(Double,Double)]
> ivBoundDeviations mx = map (deviation (0,0)) $ zip (fstLine mx) (getColumns (innerArr mx))
>	                       where --deviation :: (Double,Double) -> (Interval, [Interval]) -> (Double, Double)
>                                deviation (ls, rs) (i,[])     = ((ls - (cen i))/(cen i), (rs - (cen i))/(cen i))
>                                deviation (ls, rs) (i,(x:xs)) = deviation (ls + (lb x), rs + (rb x)) (i,xs)
>                                                                
>
>
>
> getColumns :: (Array (Int, Int) a) -> [[a]]
> getColumns mx = [[mx!(i,j)| i <- [li..ui]]| j <- [lj..uj]]
>                  where ((li, lj),(ui,uj)) = bounds mx














> {- gets dimensions and a number for a matrix and a pair of marginals, reads the files from IPFTestInput/[n]by[m]/[matricies/marginals]/[i/j].txt,
>    performs IPF on them after converting the numbers to intervals. Then writes the result with all steps in a file to 
>    IPFTestResults/[n]by[m]/Matrix[i]Marginals[j].txt
>  -}
>
> readWriteIPF :: (Int,Int) -> Int -> Int -> IO()
> readWriteIPF (n,m) i j = do mx <- readFile $ "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++ "/matricies/" ++
>                                                       show(i) ++ ".txt"
>                             mg <- readFile $ "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++ "/marginals/" ++
>                                                       show(j) ++ ".txt"
>                             writeIPF path (readDoubleMatrixToInterval mx) (readDoubleMarginalsToInterval mg)
>                               where path = "IPFTestResults/" ++
>                                             show(n) ++ "by" ++ show(m) ++ "/Matrix" ++ show(i) ++
>                                            "Marginals" ++ show(j) ++ ".txt"
>
>
> readMarginals :: (Read a, Signed a, Eq a, Num a, Show a, Fractional a) => String -> ([a],[a])
> readMarginals s = (read x, read (tail y))
>                   where (x,y) = break (=='%') s
>
> readDoubleMarginals :: String -> ([Double],[Double])
> readDoubleMarginals = readMarginals
>
> readDoubleMarginalsToInterval :: String -> ([Interval],[Interval])
> readDoubleMarginalsToInterval s = (map double2Interval mcol, map double2Interval mrow)
>                                     where (mcol, mrow) = readDoubleMarginals s
>

> readMatrix :: (Read a, Signed a, Eq a, Num a, Show a, Fractional a) => String -> (Array (Int,Int) a)
> readMatrix = twoDimListToArray.read
>
> readDoubleMatrix :: String -> (Array (Int,Int) Double)
> readDoubleMatrix = readMatrix
>
> readDoubleMatrixToInterval :: String -> (Array (Int, Int) Interval)
> readDoubleMatrixToInterval = (fmap double2Interval).readMatrix
>


> twoDimListToArray :: [[a]] -> (Array (Int,Int) a)
> twoDimListToArray l = array (bounds ) (zip (range bounds) (concat l))
>                               where  bounds = ((1,1),(length l,length (head l)))
>                           
>
> {- gets a string describing the input data,
>    an array and two lists-the marginals, runs IPF and saves the result
>    conditions: sum list1 = sum list2, arraydim1 =length list 1, arraydim2 = length list 2
>    all entries are positive, errors will be reported in output
>  -}
>
>
> writeIPF :: (Signed a, Eq a, Num a, Show a, Fractional a, MaybeDoubleCompatible a) =>
>                    FilePath -> (Array (Int,Int) a) -> ([a],[a]) -> IO ()
> writeIPF path mx (mcol, mrow)
>              | (uj - lj + 1) /= length mcol || (ui - li + 1) /= length mrow =
>                                  writeFile path "Marginal lengths do not match array bounds."
>              | sumsDontMatch (simpleSum mcol) (simpleSum mrow) =
>                                  writeFile path "Illegal marginal entries."            
>              | not (isAllPositive (fuseArr mx mcol mrow))  =
>                                  writeFile path "Some marginal or matrix entry is not positive."
>              | otherwise =
>                                  writeFile path (ipfString numIt (fuseArr mx mcol mrow))
>               where ((li,lj),(ui,uj)) = bounds mx
>				    
>                     simpleSum l = foldl maybeplus (Just 0) (map toDouble l)
> 
>                     maybeplus Nothing  _        = Nothing
>                     maybeplus (Just a) Nothing  = Nothing 
>                     maybeplus (Just a) (Just b) = Just (a + b)						
> 
>                     sumsDontMatch Nothing  x        = True
>                     sumsDontMatch (Just x) Nothing  = True
>                     sumsDontMatch (Just a) (Just b) = a /= b  
>                       
> 
>                      
>
> isAllPositive :: (Signed a, Num a) => (Array (Int,Int) a) -> Bool
> isAllPositive  mx = foldl (&&) True (map isPositive entries)
>                      where entries = [mx!(a,b) | a <- range (li, ui), b <- range (lj, uj)]
>                            ((li,lj),(ui,uj)) = bounds mx
>         
>
> {- gets an array and two marginals and (if inputs are valid) runs ipfHistory to -}
>                  
>
> saveIPF :: (Signed a, Eq a, Num a, Show a, Fractional a, MaybeDoubleCompatible a) =>
>                    (Array (Int,Int) a) -> ([a],[a]) -> Maybe [(Array (Int,Int) a)]
> saveIPF mx (mcol, mrow)
>              | (uj - lj + 1) /= length mcol || (ui - li + 1) /= length mrow = Nothing
>              | sumsDontMatch (simpleSum mcol) (simpleSum mrow) = Nothing           
>              | not (isAllPositive (fuseArr mx mcol mrow))  = Nothing 
>              | otherwise = Just (ipfHistory numIt (fuseArr mx mcol mrow))
>               where ((li,lj),(ui,uj)) = bounds mx
>				    
>                     simpleSum l = foldl maybeplus (Just 0) (map toDouble l)
> 
>                     maybeplus Nothing  _        = Nothing
>                     maybeplus (Just a) Nothing  = Nothing 
>                     maybeplus (Just a) (Just b) = Just (a + b)						
> 
>                     sumsDontMatch Nothing  x        = True
>                     sumsDontMatch (Just x) Nothing  = True
>                     sumsDontMatch (Just a) (Just b) = a /= b  
>
>
>
>
>
>
>
> -- This is supposed to go in its own file (later)
> 
> class MaybeDoubleCompatible a where
>  toDouble :: a -> Maybe Double
> 
> instance MaybeDoubleCompatible Double where
> 				toDouble f = Just f
>
> instance MaybeDoubleCompatible Float where
>               toDouble f = Just (float2Double f)
>
> instance MaybeDoubleCompatible Interval where
>               toDouble (IV a b) | a==b      = Just a
>                                 | otherwise = Nothing
>
> class Signed a where
>  isPositive :: a -> Bool
>  isNegative :: a -> Bool
>  isUnsigned :: a -> Bool
>  isUnsigned x = not $ isPositive x ||  isNegative x
> 
> instance Signed Int where
>         isPositive i = i > 0
>         isNegative i = i < 0
>
> instance Signed Double where
>         isPositive i = i > 0
>         isNegative i = i < 0
>
> instance Signed Float where
>         isPositive i = i > 0
>         isNegative i = i < 0 
>
> instance Signed Interval where
>         isPositive = isIntP
>         isNegative = isIntN
>
> 
> readDouble :: String -> Double                     
> readDouble = read
>                         
> 

> --- functions for summing up the list of doubles in certain  row or column of an array,
> 
> sumrow :: Num a => (Array (Int,Int) a) -> Int  -> a
> sumrow mx i 
>           | i < li                      = 0
>           | i > ui                      = 0
>           | otherwise                   = sum [mx!(i,j)| j <- range (lj,uj)]
>          where ((li,lj),(ui,uj)) = bounds mx
>
> sumcol :: Num a => (Array (Int,Int) a) -> Int  -> a
> sumcol mx j 
>           | j < lj                      = 0
>           | j > uj                      = 0 
>           | otherwise                   = sum [mx!(i,j)| i <- range (li,ui)]
>          where ((li,lj),(ui,uj)) = bounds mx
>
> -- one step of adapting in IPF, rowwise or columnwise, marginals are Integer

> adaptrow :: (Num a, Fractional a) => [Int] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptrow  mrow matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* fromIntegral(mrow !! i)/(sumrow matr i)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> adaptcol :: (Num a, Fractional a) => [Int] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptcol  mcol matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* fromIntegral(mcol !! j)/(sumcol matr j)) ) |
>                                         i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
> 
> {-- one step of adapting in IPF, rowwise or columnwise, marginals are Double,
>     index of mcol/mrow are relative to li,lj
> --}
> 
> adaptrow' :: (Num a, Fractional a) => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptrow'  mrow matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* (mrow !! (i-lj))/(sumrow matr i)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> adaptcol' :: (Num a, Fractional a) => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) a)
> adaptcol'  mcol matr = array (bounds matr)
>                              [((i,j), (matr!(i,j)* (mcol !! (j-lj))/(sumcol matr j)) ) |
>                                          i <- [li .. ui], j <-[lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr
>
> ipfRec :: Int -> [Int]-> [Int] -> (Array (Int,Int) Double) -> (Array (Int,Int) Double)
> ipfRec 0  _ _ mx       = mx
> ipfRec n  mrow mcol mx = adaptrow mrow (adaptcol mcol ( ipfRec (n-1) mrow mcol mx))
>
>
>
> --One-Step-IPF for Doubles, input is one array, that is split up in the process to generate
> --mrow and mcol
>
> ipf_one :: (Num a, Fractional a) => (Array (Int,Int) a) -> (Array (Int,Int) a)
> ipf_one mx =  fuseArr (adaptrow' mrow (adaptcol' mcol mx')) mcol mrow
>                    where mcol = fstLine mx
>                          mrow = fstCol mx
>                          mx' = innerArr mx
>
> ipf_one' :: (Num a, Fractional a) => (Array (Int,Int) a) -> (Array (Int,Int) a)
> ipf_one' mx =  fuseArr (adaptcol' mcol (adaptrow' mrow mx')) mcol mrow
>                    where mcol = fstLine mx
>                          mrow = fstCol mx
>                          mx' = innerArr mx
> --MultiStep-IPF for Doubles, gets number of rounds as a parameter
> 
> ipf_rec ::  Int -> (Array (Int,Int) Double) -> (Array (Int,Int) Double)
> ipf_rec 1 mx = ipf_one mx 
> ipf_rec n mx = ipf_one (ipf_rec (n-1) mx)
>                
> {- this function does a number of iterations of IPF and saves every step, outputting the list of steps -}
>
> ipfHistory :: (Num a, Show a, Fractional a) => Int -> (Array (Int,Int) a) -> [(Array (Int,Int) a)]
> ipfHistory 0 mx = [mx]
> ipfHistory n mx = mx:(ipfHistory (n-1) (ipf_one mx))
>
>
> {- this function calculates ipfHistory and then transforms all the steps into nice-looking
>    strings and concatanates them with newlines inbetween -}
> 
> ipfString :: (Num a, Show a, Fractional a) => Int -> (Array (Int,Int) a) -> String
> ipfString n = (intercalate "\n").(map printArr).(ipfHistory n)
>
>  
>
> 
> 
> 
> {-- 1. DERIVE INPUT FOR IPF FROM INPUT ARRAY:
>     - the marginal distribution of columns with 'fstLine'
>     - the marginal distribution of rows with 'fstCol'
>     - the microsample (or result from earlier round) with 'innerArr'.
>
>    fstLine: gives the li-th line of an arry of type A as a list of type A starting with entry (li,lj+1) 
>    fstCol:  gives the lj-th column of an arry of type A as a list of type A, starting with entry (li+1,lj)  
>    innerArr: returns an array mx' with bounds (li+1,lj+1)(ui,uj), s.t. mx'(i,j) = mx (i,j) when defined 
> --}
>

> fstLine :: (Array (Int,Int) a) -> [a]
> fstLine mx = [mx ! (li,j)| j <- [(lj+1) .. uj]]
>              where ((li,lj),(ui,uj)) = bounds mx
>            
>
> fstCol :: (Array (Int,Int) a) -> [a]
> fstCol mx = [mx ! (i,lj)| i <- [li+1 .. ui]]
>             where ((li,lj),(ui,uj)) = bounds mx
>
>
>
> innerArr ::  (Array (Int,Int) a) -> (Array (Int,Int) a)
> innerArr mx = array (((li+1),(lj+1)),(ui,uj))
>                         [((i,j), mx !(i,j)) |
>                             i <- [(li+1)..ui], j <- [(lj+1)..uj]]
>              where ((li,lj),(ui,uj)) = bounds mx
>
> {-- 2. PREPARE RESULTS OF IPF FOR REPETITIVE OUTPUT INPUT ARRAY
>      -- fuseArr adds to an arry the given lists, the entry at index (li-1,lj-1) is the sume of list 1
> --}
> 
> fuseArr :: Num a => (Array (Int,Int) a) -> [a] -> [a] -> (Array (Int,Int) a)
> fuseArr mx mcol mrow = array ((li-1,lj-1),(ui,uj))
>                           ([(((li-1),(lj-1)), sum mcol)] ++
>                            [(((li-1),k) , mcol !! (k-lj))| k <- [lj..uj]] ++
>                            (foldl (++) [] [(newrow i mrow mx) | i <- [li..ui]]))                          
>                         where ((li,lj),(ui,uj)) = bounds mx
>                               newrow :: Int -> [a] ->(Array (Int,Int) a) -> [((Int,Int),a)]
>                               newrow i mrow mx = ((i,(lj-1)), mrow !! (i-li)) :
>                                                   [((i,j), (mx ! (i,j))) | j <- range (lj,uj)]
>                                                    where ((li,lj),(ui,uj)) = bounds mx
>
> --                            newrow  adds the ith value from 'mrow' to the (relative) ith line
> --                            of an array 'mx' on the first position
> 
> 
>
> 
> 
>                           
> {-- 3. FOR TESTING PURPOSES
>   -- instead of calculating the results adcol produces the string expressing what should be calculated here,
>   -- adcolC mimics the results of adaptcol
> --}
> 
> adcolC :: Show a => Num a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) String)
> adcolC  mcol matr = array (bounds matr)
>                              [((i,j), ((show (matr!(i,j))) ++"*" ++ (show (mcol !! j)) ++ "/" ++
>                                        (show (sumcol matr j)))) |
>                                          i <- range (li,ui), j <- range (lj,uj)]
>                       where ((li,lj),(ui,uj)) = bounds matr
> -- is okay
> adrowC :: Show a => Num a => [a] -> (Array (Int,Int) a) -> (Array (Int,Int) String)
> adrowC  mrow matr = array (bounds matr)
>                              [((i,j), ((show (matr!(i,j))) ++"*" ++ (show (mrow !! i)) ++ "/" ++
>                                        (show (sumrow matr i)))) |
>                                          i <- [li .. ui], j <- [lj .. uj]]
>                       where ((li,lj),(ui,uj)) = bounds matr


