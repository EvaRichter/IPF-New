> module IPFInputOutput where
> import Data.Array
> import Control.Monad
> import System.Environment
> import Numeric.IEEE
> import System.IO
> import Text.Printf
> 
> -- default(Integer,Double)
>
> -- This module contains the formatting of input and output for 2dimensional IPF. Input and Output
> -- shall be handled via .txt files, such that it is readable for humans.
> -- Both data are stored as a table seperated by spaces. For an IPF with microsample of dimension
> -- mxn the table has m+1 rows and n+1 columns, with a space at index (0,0).
> -- The first line represents the marginal distribution of the columns--mcol and the first column
> -- represents the marginal distributions of the row --mrow
>
> -- transform a String seperated by spaces into a list of values of type a
> 
> readWords :: (Read a) => String -> [a]
> readWords = map read . words
>


> --- 1. INPUT: handling of an input file, and interpreting the input as an array

>
> --if the input is given in the format described above as mxn matrix, the  it will be interpreted as an array
> --with indices from 0 to m-1, and 0 to n-1. The col marginals are in the 0th line, the row marginals are
> --supposed to be represented the first column,
> --TBD so far return signs are not respected
>
> parseInput1 :: Read a => String -> Int -> Int -> (Array (Int, Int) a)
> parseInput1 str m n = (listArray ((0, 0), ((m-1), (n-1)))  matr)
>                      where  matr = readWords str
> 
> parseInput :: Read a => String ->  (Array (Int, Int) a)
> parseInput str = (listArray ((0, 0), ((m-1), (n-1)))  matr)
>                  where  matr = readWords str
>                         n    = div (length str) (m+1)
>                         m    = 10
>
> --lengthstring :: [a] -> Int
> --lengthstring str = (takeWhile (/= "\n") str)
> 
> -- length(takeWhile (/=) "\n" matr)
> --- 2. OUTPUT                                  
>
> -- write input marginals and resulting matrix as on string, seperated by spaces
> -- all entries have the length given by the parameter
>
>
> 
>
> scale :: Int -> String -> String
> scale n str = take (n - (length str)) (repeat ' ') ++ str
>
> -- given a parameter n printrow returns a list of length n of characters with length n and
> -- one newline sign in the end.print a row of lenght 

> printRow :: Show a => Int -> [a] -> String
> printRow 0 as = []
> printRow n (a:as) = (scale 43 (show a)) ++ printRow (n-1) as
>
> -- given an array with dimension mxn printArr returns a string, where the lines are seperated "\n"
>
>
> printArr :: Show a => (Array (Int, Int) a) -> String
> printArr mx = concat [(printRow (uj+1) [mx!(i,j)| j <- range (lj,uj)] ++ "\n")|i<- range (li,ui)]
>               where ((li,lj),(ui,uj)) = bounds mx
>

>                                 
> 
