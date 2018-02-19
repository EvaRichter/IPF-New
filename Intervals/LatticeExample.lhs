> module LatticeExample where
> import Numeric.IEEE

> {--
> data Four = P1|P2|P3|P4
>             deriving Eq
>
> 
> 
> data Power4 = Empty |Si Four|  Do  Four  Four |  Tr Four Four Four | All
>
> constP4 :: Four -> Four -> Power4
> constP4 x x = Si x
> constP4 x y = Do x y
>
> comp2 :: [Four] -> [Four] -> [Four]
> comp2 []_ = []
> comp2 _ [] = []
> -- comp2 (x : xs)(ys) | isElem x ys = x : (comp2 xs ys)
> --                   | otherwise   = (comp2 xs ys)
>

> {--
> me :: Power4 -> Power4 -> Power4
> me x All = x
> me All x = x
> me (Si x) (Si y) = 
> me (Do x x') (Do y y') |(comp2 == []) = Empty
>                        | comp2 == [a] = Si a
                         | otherwise    = Do x x'
> --}
