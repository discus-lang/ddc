

data Eval
        = Eval
        { evalPrim      :: k -> [Env O b u k] -> Maybe (Exp O b u k)
        , evalSubst     :: [(b, u)] -> Exp a b u k -> Exp a b u k }


eval    :: a
        -> (k -> [Exp a b u k] -> Maybe (Exp a b u k))
        -> ()
        -> Env b u
        -> Exp a b u k 
        -> Exp a b u k

eval o prim env xx
 = case xx of
        XLit _        
         -> xx

        -- Evaluate a closed computation.
        XAbs   _ bs xsBind xBody
         | length bs == length xsBind
         = let  env'    = envApp env (envMake bs xsBind)
                xsBind' = map (eval prim env')   xsBind
                env''   = envApp env (envMake bs xsBind')
           in   eval prim env'' xBody

        -- Apply a literal.
        XApp   _ (XLit _ k) us
         -> fromMaybe xx
         $  prim k $ fromMaybe xx
                   $ sequence 
                   $ map (envLookup env) us

        -- Apply an abstraction.
        XApp   _ (XAbs _ bs xsBind xBody) us
         = let  
                -- Number of unsaturated binders.
                nParams = length bs - length xsBind

                -- How many binders to saturate.
                nSat    = max nBinds (length us)

                -- Split the binders and args into the ones we're 
                -- going to satisfy now.
                (bsHere, bs')   = splitAt nSat
                (usHere, us')   = splitAt nSat us

                -- Instantiate the bindings and body.
                xsBind' = map (subst bsHere usHere) xsBind
                xBody   = subst bsHere usHere xBody

                sub     = subst (zip bsHere usHere)
                abs'    = XAbs o bs' (map sub xsBind) (sub xBody)

           in   if length us == 0
                 then abs'
                 else XApp o abs' us'

