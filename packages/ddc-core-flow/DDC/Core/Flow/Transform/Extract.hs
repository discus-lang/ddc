
module DDC.Core.Flow.Transform.Extract
        ( extractModule
        , extractProcedure)
where
import DDC.Core.Flow.Transform.Extract.Intersperse
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Transform.Annotate
import DDC.Core.Module


-- | Extract a core module from some stream procedures.
--   This produces vanilla core code again.
extractModule    :: ModuleF -> [Procedure] -> ModuleF
extractModule orig procs
        = orig
        { moduleBody    = annotate () $ extractTop procs }


-- | Extract a top level binding from a procedure.
extractTop       :: [Procedure] -> ExpF
extractTop procs
 = XLet (LRec (map extractProcedure procs)) xUnit


-- | Extract code for a whole procedure.
extractProcedure  :: Procedure -> (Bind Name, ExpF)
extractProcedure (Procedure n bsParam xsParam nest tResult xResult)
 = let  tBody   = foldr tFun    tResult $ map typeOfBind xsParam
        tQuant  = foldr TForall tBody   $ bsParam
   in   ( BName n tQuant
        ,   xLAMs bsParam
          $ xLams xsParam
          $ extractNest nest [] xResult )


-------------------------------------------------------------------------------
-- | Extract code for a loop nest.
extractNest 
        :: Nest                 -- ^ Loops to run in sequence.
        -> [LetsF]              -- ^ Baseband statements from the source program
                                --   that run after the loop operators.
        -> ExpF                 -- ^ Final result of procedure.
        -> ExpF

extractNest nest stmts xResult
 = let stmts'   = intersperseStmts (extractLoop nest) stmts
   in  xLets stmts' xResult


-------------------------------------------------------------------------------
-- | Extract code for a possibly nested loop.
extractLoop      :: Nest -> [LetsF]

-- Code in a loop context.
extractLoop (NestLoop tRate starts bodys inner ends _result)
 = let  
        -- Starting statements.
        lsStart = concatMap extractStmtStart starts

        -- The loop itself.
        lLoop   = LLet  (BNone tUnit)
                        (xApps (XVar (UPrim (NameOpControl OpControlLoop) 
                                            (typeOpControl OpControlLoop)))
                                [ XType tRate           -- loop rate
                                , xBody ])              -- loop body

        -- The worker passed to the loop# combinator.
        xBody   = XLam  (BAnon tNat)                    -- loop counter.
                $ xLets (lsBody ++ lsInner)
                           xUnit

        -- Process the elements.
        lsBody  = concatMap extractStmtBody bodys

        -- Handle inner contexts.
        lsInner = extractLoop inner

        -- Ending statements 
        lsEnd   = concatMap extractStmtEnd ends

   in   lsStart ++ [lLoop] ++ lsEnd

-- Code in a select / if context.
extractLoop (NestIf _tRateOuter tRateInner uFlags stmtsBody nested)
 = let
        -- Get the name of a single flag from the series of flags.
        UName nFlags    = uFlags
        nFlag           = NameVarMod nFlags "elem"
        xFlag           = XVar (UName nFlag)

        -- Make a name for the counter.
        TVar (UName nK) = tRateInner
        uCounter        = UName (NameVarMod nK "count")

        xBody           = xGuard xFlag (XVar uCounter)
                          (  XLam (BAnon tNat)
                          $ xLets (lsBody ++ lsNested) xUnit)

        -- Selector context.
        lsBody   = concatMap extractStmtBody stmtsBody

        -- Nested contexts.
        lsNested = extractLoop nested

  in    [LLet (BNone tUnit) xBody]


extractLoop NestEmpty
 = []

extractLoop (NestList nests)
 = concatMap extractLoop nests


-------------------------------------------------------------------------------
-- | Extract loop starting code.
--   This comes before the main loop.
extractStmtStart :: StmtStart -> [LetsF]
extractStmtStart ss
 = case ss of
        -- Evaluate a pure expression.
        StartStmt b x
         -> [LLet b x]

        -- Allocate a new vector.
        StartVecNew nVec tElem tRate'
         -> [LLet (BName nVec (tVector tElem))
                  (xNewVectorR tElem tRate') ]


        -- Initialise the accumulator for a reduction operation.
        StartAcc n t x    
         -> [LLet (BName n (tRef t)) 
                  (xNew t x)]        


-------------------------------------------------------------------------------
-- | Extract loop body code.
extractStmtBody :: StmtBody -> [LetsF]
extractStmtBody sb
 = case sb of
        BodyStmt b x
         -> [ LLet b x ]

        -- Write to a vector.
        BodyVecWrite nVec tElem xIx xVal
         -> [ LLet (BNone tUnit)
                   (xWriteVector tElem (XVar (UName nVec)) xIx xVal)]

        -- Read from an accumulator.
        BodyAccRead  n t bVar
         -> [ LLet bVar
                   (xRead t (XVar (UName n))) ]

        -- Accumulate an element from a stream.
        BodyAccWrite nAcc tElem xWorker    
         -> [ LLet (BNone tUnit)
                   (xWrite tElem (XVar (UName nAcc)) xWorker)]


-------------------------------------------------------------------------------
-- | Extract loop ending code.
--   This comes after the main loop.
extractStmtEnd :: StmtEnd -> [LetsF]
extractStmtEnd se
 = case se of
        EndStmt b x
         -> [LLet b x]

        -- Read the accumulator of a reduction operation.
        EndAcc n t nAcc 
         -> [LLet (BName n t) 
                  (xRead t (XVar (UName nAcc))) ]

        -- Slice.
        EndVecSlice nVec tElem tRate 
         -> let 
                -- Get the name of the counter.
                TVar (UName nK) = tRate
                uCounter        = UName (NameVarMod nK "count")
                xCounter        = xRead tNat (XVar uCounter)
                xVec            = XVar (UName nVec)

                -- Read the counter in a let since it will need to be threaded
           in   [ LLet  (BAnon      tNat)
                        xCounter

                , LLet  (BName nVec (tVector tElem)) 
                        (xSliceVector tElem (XVar (UIx 0)) xVec) ]

