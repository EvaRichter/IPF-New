> module Clean where
> import Algebra.Lattice
> import Data.List
>
> 
> 
> meet' :: (Eq a, BoundedJoinSemiLattice a, MeetSemiLattice a ) => a -> a -> [a]
> meet' x y | (meet x y) == bottom = []
>           | otherwise            = [(/\) x y]
>
> 
> clean :: (Eq a, BoundedJoinSemiLattice a, MeetSemiLattice a ) => [a] -> [a]
> clean (z : zs) = (foldl join  z zsdm) : zsdn
>                     where
>                      zsd = clean zs 
>                      (zsdm, zsdn)  = partition (not.null.(meet' z)) zsd
>
> data Four = 1|2|3|4
> data Power4 = 
    Empty :: Power4 
    Sing :: Four -> Power4
    Doub :: Four -> Four -> Power
