
module DDC.Core.Flow.Transform.Extract
        (extractModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Exp.Procedure
import DDC.Core.Flow.Name


-- | Extract a core module from some stream procedures.
--   This produces vanilla core code again.
extractModule    :: Module () Name -> [Procedure] -> Module () Name
extractModule orig procs
        = orig
        { moduleBody    = extractTop procs }


extractTop       :: [Procedure] -> Exp () Name
extractTop procs
 = XLet () (LRec (map extractProcedure procs)) (xUnit ())


extractProcedure  :: Procedure -> (Bind Name, Exp () Name)
extractProcedure (Procedure n t paramTypes paramValues loop)
 = ( BName n t
   ,   xLAMs () paramTypes
     $ xLams () paramValues
     $ extractLoop loop )


extractLoop      :: Loop -> Exp () Name
extractLoop (Loop _context starts bodys _nested ends result)
 =  xLets ()
        (  concatMap extractStmtStart starts
        ++ concatMap extractStmtBody  bodys
        ++ concatMap extractStmtEnd   ends)     -- TODO: add nested
        result


extractStmtStart :: StmtStart -> [Lets () Name]
extractStmtStart ss
 = case ss of
        StartAcc n t x    
         -> [LLet LetStrict (BName n t) x]        


extractStmtBody  :: StmtBody  -> [Lets () Name]
extractStmtBody sb
 = case sb of
        BodyAcc n t _nStream xWorker    
         -> [LLet LetStrict (BName n t) xWorker]


extractStmtEnd   :: StmtEnd   -> [Lets () Name]
extractStmtEnd se
 = case se of
        EndStmts ss     -> map extractStmt ss
        EndAcc n t nAcc 
         -> [LLet LetStrict (BName n t) (XVar () (UName nAcc)) ]


extractStmt       :: Stmt -> Lets () Name
extractStmt (Stmt b x)
 = LLet LetStrict b x
 
