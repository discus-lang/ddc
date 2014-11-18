module DDC.Core.Flow.Transform.Slurp
        ( slurpProcesses
        , slurpOperator
        , isSeriesOperator
        , isVectorOperator)
where
import DDC.Core.Flow.Transform.Slurp.Context
import DDC.Core.Flow.Transform.Slurp.Operator
import DDC.Core.Flow.Transform.Slurp.Error
import DDC.Core.Flow.Transform.Slurp.Resize
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Transform.Deannotate
import DDC.Core.Module
import qualified DDC.Type.Env           as Env
import DDC.Type.Env                     (TypeEnv)
import Data.List
import qualified Data.Map               as Map


-- | Slurp stream processes from the top level of a module.
slurpProcesses :: Module () Name -> Either Error [Either Process (Bind Name, Exp () Name)]
slurpProcesses mm
 = slurpProcessesX (deannotate (const Nothing) $ moduleBody mm)


-- | Slurp stream processes from a module body.
--   A module consists of some let-bindings wrapping a unit data constructor.
slurpProcessesX :: Exp () Name   -> Either Error [Either Process (Bind Name, Exp () Name)]
slurpProcessesX xx
 = case xx of
        -- Slurp processes definitions from the let-bindings.
        XLet lts x'
          -> do ps1     <- slurpProcessesLts lts 
                ps2     <- slurpProcessesX x'
                return  $ ps1 ++ ps2

        -- Ignore the unit data constructor at the end of the module.
        _
         | xx == xUnit  -> Right []
         | otherwise    -> Left $ ErrorBadProcess xx


-- | Slurp stream processes from the top-level let expressions.
slurpProcessesLts :: Lets () Name -> Either Error [Either Process (Bind Name, Exp () Name)]
slurpProcessesLts (LRec binds)
 = sequence [slurpProcessLet b x | (b, x) <- binds]

slurpProcessesLts (LLet b x)
 = sequence [slurpProcessLet b x]

slurpProcessesLts _
 = return []


-------------------------------------------------------------------------------
-- | Slurp stream operators from a top-level binding.
slurpProcessLet 
        :: Bind Name            -- ^ Binder for the whole process.
        -> Exp () Name          -- ^ Expression of body.
        -> Either Error (Either Process (Bind Name, Exp () Name))

slurpProcessLet (BName n t) xx

 -- We assume that all type params come before the value params.
 | Just (NameTyConFlow TyConFlowProcess, [tProc, tLoopRate])
        <- takePrimTyConApps $ snd $ takeTFunAllArgResult t
 , Just (fbs, xBody)    <- takeXLamFlags xx
 = let  
        -- Split binders into type and value binders.
        (fbts, fbvs)    = partition fst fbs

        -- Type binders.
        bts             = map snd fbts

        -- Value binders.
        bvs             = map snd fbvs

        -- Slurp the body of the process.
   in do
        ctx     <- slurpProcessX Env.empty Map.empty Map.empty xBody

        return  $ Left
                $ Process
                { processName          = n
                , processProcType      = tProc
                , processLoopRate      = tLoopRate
                , processParamTypes    = bts
                , processParamValues   = bvs

                , processContext       = ctx }

slurpProcessLet b xx
 = return $ Right (b, xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest. 
-- 
--   The process type environment records what process bindings are in scope,
--   so that we can check that the overall process is well formed. 
--   This environment only needs to contain locally defined process variables,
--   not the global environment for the whole module.
--
slurpProcessX 
        :: TypeEnv Name         -- ^ Process type environment.
        -> Map.Map Name Context -- ^ Contexts of in-scope
        -> Map.Map Name Resize  -- ^ Resizes of in-scope
        -> ExpF                 -- ^ A sequence of non-recursive let-bindings.
        -> Either Error Context

slurpProcessX tenv ctxs rs xx
 | XLet (LLet b x) xMore        <- xx
 = do   
        -- Slurp operators from the binding.
        (ctxs', rs')            <- slurpBindingX tenv ctxs rs b x

        -- If this binding defined a process then add it do the environment.
        let tenv'
                | isProcessType $ typeOfBind b  = Env.extend b tenv
                | otherwise                     = tenv

        -- Slurp the rest of the process using the new environment.
        slurpProcessX tenv' ctxs' rs' xMore

-- Slurp a process ending.
slurpProcessX tenv ctxs rs xx
 -- The process ends with a variable that has Process# type.
 | XVar u       <- xx
 , Just t       <- Env.lookup u tenv
 , isProcessType t
 , UName u'     <- u
 , Just c       <- Map.lookup u' ctxs
 = return c

 -- The process ends by joining two existing processes.
 -- We assume that the overall expression is well typed.
 | Just (NameOpSeries OpSeriesJoin, [_, _, XVar (UName a), XVar (UName b)])
                <- takeXPrimApps xx
 , Just a'      <- Map.lookup a ctxs
 , Just b'      <- Map.lookup b ctxs
 = return (merge a' b')

 -- Process finishes with some expression that doesn't look like it 
 -- actually defines a value of type Process#.
 | otherwise
 = Left (ErrorBadProcess xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from a let-binding.
slurpBindingX 
        :: TypeEnv Name         -- ^ Process type environment.
        -> Map.Map Name Context -- ^ Contexts of in-scope
        -> Map.Map Name Resize  -- ^ Resizes of in-scope
        -> BindF                -- ^ Binder to assign result to.
        -> ExpF                 -- ^ Right of the binding.
        -> Either 
                Error
                ( Map.Map Name Context
                , Map.Map Name Resize )


-- Decend into more let bindings.
-- We get these when entering into a nested context.
slurpBindingX tenv ctxs rs b1 xx
 | XLet (LLet b2 x2) xMore      <- xx
 = do   
        -- Slurp operators from the binding.
        (ctxs', rs')            <- slurpBindingX tenv ctxs rs b2 x2

        -- If this binding defined a process then add it to the environement.
        let tenv'
                | isProcessType $ typeOfBind b2 = Env.extend b2 tenv
                | otherwise                     = tenv

        -- Slurp the rest of the process using the new environment.
        slurpBindingX tenv' ctxs' rs' bs b1 xMore


-- Slurp a series#
-- This creates a new context
slurpBindingX _tenv ctxs rs b@(BName n _)
 (   takeXPrimApps 
  -> Just ( NameOpSeries OpSeriesSeriesOfRateVec
          , [ XType _tProc
            , XType tK
            , XType tA
            , XVar vec]))
 = do
        let op          = OpSeriesOfRateVec
                        { opResultSeries        = b
                        , opInputRate           = tK
                        , opInputRateVec        = vec 
                        , opElemType            = tA }

        let context     = ContextRate
                        { contextRate           = tK
                        , contextOps            = [op]
                        , contextInner          = [] }

        let ctxs' = Map.insert n context ctxs 

        return (ctxs', rs)


-- Slurp a mkSel1#
-- This creates a nested selector context.
slurpBindingX tenv ctxs rs b@(BName n _)
 (   takeXPrimApps 
  -> Just ( NameOpSeries (OpSeriesMkSel 1)
          , [ XType tProc
            , XType tK1
            , XVar  uFlags@(UName nFlags)

            , XLAM         (BName nR kR)
             (XLam    bSel@(BName nSel _)
                      xBody)]))
 | kR == kRate
 = do
        flagsContext   <- lookupOrDie nFlags ctxs

        -- Introduce new series with name of selector,
        -- as copy of flags series
        let uSelCopy    = UName nSel
        let bSelCopy    = BName nSel (tSeries tProc tK1 tBool)

        let opId        = OpId
                        { opResultSeries        = bSelCopy
                        , opInputRate           = tK1
                        , opInputSeries         = uFlags
                        , opElemType            = tBool }

        let context     = ContextSelect
                        { contextOuterRate      = tK1
                        , contextInnerRate      = TVar (UName nR)
                        , contextFlags          = uFlags
                        , contextSelector       = bSel
                        , contextOps            = [opId]
                        , contextInner          = [] }

        context'       <- insertContext context  flagsContext
        let ctxs'       = Map.insert  n context' ctxs

        slurpBindingX tenv ctxs' rs b xBody


-- Slurp a mkSel1#
-- This creates a nested selector context.
slurpBindingX tenv ctxs rs b@(BName n _)
 (   takeXPrimApps 
  -> Just ( NameOpSeries OpSeriesMkSegd
          , [ XType tProc
            , XType tK1
            , XVar  uLens@(UName nLens)

            , XLAM          (BName nR    kR)
             (XLam    bSegd@(BName nSegd _)
                      xBody)]))
 | kR == kRate
 = do
        lensContext    <- lookupOrDie nLens ctxs

        -- Introduce new series with name of segd,
        -- as copy of lens series
        let uSegdCopy   = UName nSegd
        let bSegdCopy   = BName nSegd (tSeries tProc tK1 tNat)

        let opId        = OpId
                        { opResultSeries        = bSegdCopy
                        , opInputRate           = tK1
                        , opInputSeries         = uLens
                        , opElemType            = tNat }

        let context     = ContextSelect
                        { contextOuterRate      = tK1
                        , contextInnerRate      = TVar (UName nR)
                        , contextLens           = uLens
                        , contextSegd           = bSegd
                        , contextOps            = [opId]
                        , contextInner          = [] }

        context'       <- insertContext context  lensContext
        let ctxs'       = Map.insert  n context' ctxs

        slurpBindingX tenv ctxs' rs b xBody


-- Slurp a series operator that doesn't introduce a new context.
slurpBindingX _ ctxs rs b@(BName n _) xx
 | Just (ins, k,op)  <- slurpOperator b xx
 = do   ins'           <- mapM (flip lookupOrDie ctxs) ins

        let ctx         = ContextRate
                        { contextRate           = k
                        , contextOps            = [op]
                        , contextInner          = [] }

        let go []     c = c
            go (i:is) c = go ns <$> insertContext i c

        context'       <- go ins' ctx
        let ctxs'       = Map.insert n context' ctxs
        return (ctxs', rs)

-- Slurp an append operator
slurpBindingX _ ctxs rs b@(BName n _) xx
 | Just (NameOpSeries OpSeriesAppend
        , [ XType _P, XType tK1, XType tK2, XType tA
          , XVar (UName nIn1), XVar (UName nIn2) ] ) 
                                <- takeXPrimApps xx
 = do   in1'           <- lookupOrDie nIn1 ctxs
        in2'           <- lookupOrDie nIn2 ctxs

        let ctx         = ContextAppend
                        { contextRate1          = tK1
                        , contextInner1         = in1'
                        , contextRate2          = tK2
                        , contextInner2         = in2' }

        let ctxs'       = Map.insert n ctx ctxs
        return (ctxs', rs)


-- Slurp a Resize
slurpBindingX _ ctxs rs b@(BName n _) xx
 | Just rr <- seqEitherMaybe $ slurpResize ctxs xx
 = do   r  <- rr
        return (ctxs, Map.insert n r rs)


-- Slurp a process ending.
slurpBindingX tenv ctxs rs _ xx
 -- The process ends with a variable that has Process# type.
 | XVar u       <- xx
 , Just t       <- Env.lookup u tenv
 , isProcessType t
 = return ([], [])                

 -- The process ends by joining two existing processes.
 -- We assume that the overall expression is well typed.
 | Just (NameOpSeries OpSeriesJoin, [_, _, _, _])
                <- takeXPrimApps xx
 = return ([], [])

 -- Process finishes with some expression that doesn't look like it 
 -- actually defines a value of type Process#.
 | otherwise
 = Left (ErrorBadOperator xx)

