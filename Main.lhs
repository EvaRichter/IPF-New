> module Main where
> import Data.Array
> import Control.Monad
> import System.Environment
> import Numeric.IEEE
> import System.IO
> import IPFInputOutput
> import Examples
> 
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalOps
> import Intervals.IntervalProp
> 
> 
> 
> main :: IO ()
> main = do
>       inh <- openFile "test3x3.txt" ReadMode
>       outh <- openFile "output1.txt" WriteMode
>
>       writeFile "outputtmx4.txt" (ipfString 6 (fmap double2Int tmx4))
>        
>       mainloop inh outh []
>       hClose inh
>       hClose outh
>
> mainloop :: Handle -> Handle -> [String] -> IO ()
> mainloop inh outh lstr = 
>    do ineof <- hIsEOF inh
>       if ineof
>           then do
>                let ri = length lstr
>                let ci = length (words (head lstr))
>                let mtr = array ((0,0),((ri-1),(ci-1)))
>                                [((i,j), readDouble $ words (lstr !! i) !! j)|
>                                          i <- range (0,(ri-1)),
>                                          j <- range (0,(ci-1))]
>                
>                foldr (>>)(return()) (map (hPutStrLn outh) lstr)
>                print ri
>                print ci
>                print mtr
>           else do inpStr  <- hGetLine inh                                    
>                   mainloop inh outh (lstr++[inpStr])
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
>
> ipfString :: (Num a, Fractional a, Show a) => Int -> (Array (Int,Int) a) -> String
> ipfString 0 mx = printArr mx
> ipfString n mx = printArr mx ++ "\n" ++ ipfString (n-1) (ipf_one mx)
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


