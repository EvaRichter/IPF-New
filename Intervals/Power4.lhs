> module Power4 where
> import Numeric.IEEE
> import Data.List

> data Four = P1|P2|P3|P4
>             deriving Eq

> data Power4 = Empty |Si Four|  Do  Four  Four |  Tr Four Four Four | All

> eq4 :: Power4 -> Power4 -> Bool
> eq4 Empty Empty = True
> eq4 (Si x) (Si y) =  (x == y)
> eq4 (Do x y) (Si z) = (x == z) && (y == z)
> eq4 (Do x y) (Do x' y') = (elem  x [x',y']) && elem y [x',y']
> eq4 (Tr x y z)(Tr x' y' z')= (elem x [x',y',z']) && elem y 
