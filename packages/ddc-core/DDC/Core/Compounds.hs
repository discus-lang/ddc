
module DDC.Core.Compounds 
        (takeXLams)
where
import DDC.Core.Exp


-- | Split nested lambdas from the front of an expression
--   or `Nothing` if there was no outer lambda
takeXLams :: Exp a p n -> Maybe ([Bind n], Exp a p n)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)
