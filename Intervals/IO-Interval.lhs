> module IOInterval where
> import Numeric.IEEE
> import IntervalType
> import Control.Monad
>
>


>-- instance Read Interval where
>--  readsPrec d r = do {                                      
>--                   ("[",r')<- lex r;
>--                   (a,s) <- readsPrec d r';
>--                   ("O",t) <- lex s;
>--                   (b,w) <- readsPrec d t;
>--                   ("]",u)<- lex w;
>--                   return ((IV a b),u)
>--                     }
>

