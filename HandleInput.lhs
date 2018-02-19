> module IPF_TwoDim.HandleInput where
> import System.IO  
> import Control.Monad

> main = do  
>        let list = []
>        handle <- openFile "IPF_TwoDim/test3x3.txt" ReadMode
>        contents <- hGetContents handle
>        let singlewords = words contents
>            list = f singlewords
>        print list
>        hClose handle   

> f :: [String] -> [Int]
> f = map read
