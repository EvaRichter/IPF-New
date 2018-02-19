> module Examples where
> import Data.Array
> -- import Control.Monad
> -- import System.Environment
> -- import Numeric.IEEE
> -- import System.IO
> -- import IPFInputOutput

-- testinputs, 3x3 matrix and two lists with length 3

> matrix :: (Array (Int,Int) Double)
> matrix =  array ((1,1),(3,2)) [((i,j), (fromIntegral (i*j))) | i <- [1..3], j <- [1,2]]
>
> mulx :: (Array (Int,Int) Double)
> mulx =  array ((1,1),(8,9)) [((i,j), (fromIntegral (i*j))) | i <- [1..8], j <- [1..9]]
> 
> tmc :: [Int]
> tmc = [11,9,8]
>
> tmr :: [Int]
> tmr = [5,15,8]
>
> tmx :: (Array (Int,Int) Double)
> tmx = array ((0,0),(2,2))[((0,0),1),((0,1),2),((0,2),3),
>                           ((1,0),10),((1,1),99),((1,2),12), 
>                           ((2,0),20),((2,1),21),((2,2),22)]
> tmx4 :: (Array (Int,Int) Double)
> tmx4 = array ((0,0),(3,3))[((0,0),266),((0,1),85),((0,2),62),((0,3),119),
>                           ((1,0),80),((1,1),1),((1,2),2),((1,3),3),
>                           ((2,0),77),((2,1),11),((2,2),12),((2,3),13),
>                           ((3,0),109),((3,1),21),((3,2),22),((3,3),23)]
>
>  -- testinputs, 2x2 matrix and two lists with length 2
> tmc1 :: [Double]
> tmc1 = [0,1]
>
> tmr1 :: [Double]
> tmr1 = [10,20]
>
> tmx1 :: (Array (Int,Int) Double)
> tmx1 = array ((0,0),(1,1))[((0,0),11),((0,1),12),
>                           ((1,0),21),((1,1),22)]
