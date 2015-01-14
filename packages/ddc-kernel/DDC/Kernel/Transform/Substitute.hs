
module DDC.Kernel.Transform.Substitute
        ( Skip (..)
        , substitute)
where
import DDC.Kernel.Exp


-- | A bound annotated with a skip counter.
--
--   A skip counter is natural number that behaves like a debruijn index.
--
--   If it is non-zero then it counts the number of matching binders we
--   should skip over on the way up the expression tree before reaching
--   the binder that this bound is associated with.
--
data Skip u
        = Skip u Int

instance Show u => Show (Skip u) where
        show (Skip u i) = show u ++ "^" ++ show i


-- | Substitute some bounds into an expression.
substitute   
        :: Eq b
        => o                    -- ^ Annotation for created nodes.
        -> (u -> b -> Bool)     -- ^ Check if a bound matches a bind.
        -> [(Maybe b, Skip u)]  -- ^ Substitute this environment.
        -> Exp o b (Skip u) k   -- ^ Into this.
        -> Exp o b (Skip u) k

substitute o match bus0 xx0
 = down bus0 xx0
 where  
        -- Get a bound from the environment.
        get bus su@(Skip u _)
         = case [su' | (Just b, su'@(Skip u' _)) <- bus, match u b] of
                [su']   -> su'
                _       -> su

        -- Check if a bind would capture a bound, 
        --   and update the skip counter if it would.
        skip bs (mb@(Just b), su@(Skip u i))
         = let  mb'     = if any (== b) bs    then Nothing else mb
                i'      = if any (match u) bs then i + 1   else i
           in   (mb', Skip u i')

        -- Decend into the expression.
        down bus xx
         = case xx of
                XLit{}        -> xx

                XAbs _ bs xsBind xBody
                 -> let bus'    = map (skip bs) bus
                        sub     = down bus'
                    in  XAbs o bs (map sub xsBind) (sub xBody)

                XApp _ xAbs usArg
                 -> let xAbs'   = down bus xAbs
                        usArg'  = map (get bus) usArg
                    in  XApp o xAbs' usArg'
