
-- | Put our extended LLVM AST in the form that the LLVM compiler will accept directly.
--
--   * inline `ISet` meta-instructions
--   * drop `INop` meta-instructions
--   * propagate calling conventions from declarations to call sites.
--   * Flatten out `XConv` and `XGet` expressions into separate instructinos.
--
module DDC.Llvm.Transform.Clean
        (clean)
where
import DDC.Llvm.Syntax
import DDC.Control.Monad.Check
import Data.Map                 (Map)
import Data.Sequence            (Seq, (|>))
import qualified Data.Map       as Map
import qualified Data.Foldable  as Seq
import qualified Data.Sequence  as Seq

-- | Clean a module.
clean :: Module -> Module
clean mm
 = let  binds           = Map.empty
        Right funcs'    = evalCheck 0 (mapM (cleanFunction mm binds) $ modFuncs mm) 
   in   mm { modFuncs   = funcs' }


-- | Clean a function.
cleanFunction
        :: Module               -- ^ Module being cleaned.
        -> Map Var Exp          -- ^ Map of variables to their values.
        -> Function -> CleanM Function

cleanFunction mm binds fun
 = do   
        blocks' <- cleanBlocks mm binds Map.empty []
                $  funBlocks fun

        return  $ fun { funBlocks = blocks' }


-- | Clean set instructions in some blocks.
cleanBlocks 
        :: Module               -- ^ Module being cleaned.
        -> Map Var Exp          -- ^ Map of variables to their values.
        -> Map Var Label        -- ^ Map of variables to the label 
                                --    of the block they were defined in.
        -> [Block]              -- ^ Accumulated blocks.
        -> [Block]              -- ^ Blocks still to clean.
        -> CleanM [Block]

cleanBlocks _mm _binds _defs acc []
        = return $ reverse acc

cleanBlocks mm binds defs acc (Block label instrs : bs) 
 = do   (binds', defs', instrs2) 
                <- cleanInstrs mm label binds defs Seq.empty
                $  Seq.toList instrs

        let instrs' = Seq.fromList instrs2
        let block'  = Block label instrs'

        cleanBlocks mm binds' defs' (block' : acc) bs


-- | Clean set instructions in some instructions.
cleanInstrs 
        :: Module               -- ^ Module being cleaned.
        -> Label                -- ^ Label of the current block.
        -> Map Var Exp          -- ^ Map of variables to their values.
        -> Map Var Label        -- ^ Map of variables to the label
                                --    of the block they were defined in.
        -> Seq AnnotInstr       -- ^ Accumulated instructions.
        -> [AnnotInstr]         -- ^ Instructions still to clean.
        -> CleanM (Map Var Exp, Map Var Label, [AnnotInstr])

cleanInstrs _mm _label binds defs acc []
        = return (binds, defs, Seq.toList acc)

cleanInstrs mm label binds defs acc (ins@(AnnotInstr i annots) : instrs)
  = let next binds' defs' acc' 
                   = cleanInstrs mm label binds' defs' acc' instrs

        reAnnot i' = annotWith i' annots

        sub xx  
         = case xx of
                XVar v
                  | Just x' <- Map.lookup v binds
                  -> sub x'
                _ -> xx

    in case i of
        IComment{}              
         -> next binds defs 
         $  acc |> ins

        -- The LLVM compiler doesn't support ISet instructions,
        --  so we inline them into their use sites.
        ISet v x                
         -> let binds'  = Map.insert v x binds
            in  next binds' defs acc

        -- The LLVM compiler doesn't support INop instructions,
        --  so we drop them out.         
        INop
         -> next binds defs acc

        -- At phi nodes, drop out joins of the 'undef' value.
        --  The converter adds these in rigtht before calling 'abort',
        --  so we can never arrive from one of those blocks.
        IPhi v xls
         -> let 
                -- Don't merge undef expressions in phi nodes.
                keepPair (XUndef _)  = False
                keepPair _           = True

                i'      = IPhi v [(sub x, l) 
                                        | (x, l) <- xls 
                                        , keepPair (sub x) ]

                defs'   = Map.insert v label defs
            in  next binds defs' $ acc |> reAnnot i'

        IReturn Nothing
         -> next binds defs 
         $  acc |> ins

        IReturn (Just x)
         -> next binds defs 
         $  acc |> (reAnnot $ IReturn (Just (sub x)))

        IBranch{}
         -> next binds defs
         $  acc |> ins

        IBranchIf x l1 l2
         -> next binds defs
         $  acc |> (reAnnot $ IBranchIf (sub x) l1 l2)

        ISwitch x def alts
         -> next binds defs
         $  acc |> (reAnnot $ ISwitch   (sub x) def alts)

        IUnreachable
         -> next binds defs
         $  acc |> ins

        IOp    v op x1 x2
         |  defs'        <- Map.insert v label defs
         -> next binds defs'
         $  acc |> (reAnnot $ IOp   v op (sub x1) (sub x2))

        IConv  v c x
         |  defs'        <- Map.insert v label defs
         -> next binds defs'
         $  acc |> (reAnnot $ IConv v c (sub x))

        ILoad  v x
         |  defs'        <- Map.insert v label defs
         -> next binds defs'
         $  acc |> (reAnnot $ ILoad v   (sub x))

        IStore x1 x2
         -> next binds defs
         $  acc |> (reAnnot $ IStore    (sub x1) (sub x2))

        IICmp  v c x1 x2
         |  defs'        <- Map.insert v label defs
         -> next binds defs'
         $  acc |> (reAnnot $ IICmp v c (sub x1) (sub x2))

        IFCmp  v c x1 x2
         |  defs'        <- Map.insert v label defs
         -> next binds defs'
         $  acc |> (reAnnot $ IFCmp v c (sub x1) (sub x2))

        ICall  (Just v) ct mcc t n xs ats
         |  defs'        <- Map.insert v label defs
         -> let Just cc2 = callConvOfName mm n
                cc'      = mergeCallConvs mcc cc2
            in  next binds defs'
                 $ acc |> (reAnnot $ ICall (Just v) ct (Just cc') t n (map sub xs) ats) 

        ICall  Nothing ct mcc t n xs ats
         -> let Just cc2 = callConvOfName mm n
                cc'      = mergeCallConvs mcc cc2
            in  next binds defs
                 $ acc |> (reAnnot $ ICall Nothing  ct (Just cc') t n (map sub xs) ats) 


-- | Lookup the calling convention for the given name.
callConvOfName :: Module -> Name -> Maybe CallConv
callConvOfName mm name
        -- Functions defined at top level can have different calling
        -- conventions.
        | NameGlobal str <- name
        , Just cc2       <- lookupCallConv str mm
        = Just cc2

        -- Unknown functions bound to variables are assumed to have
        -- the standard calling convention.
        | NameLocal _    <- name 
        = Just CC_Ccc

        | otherwise      = Nothing


-- | If there is a calling convention attached directly to an ICall
--   instruction then it must match any we get from the environment.
mergeCallConvs :: Maybe CallConv -> CallConv -> CallConv
mergeCallConvs mc cc
 = case mc of
        Nothing         -> cc
        Just cc'        
         | cc == cc'    -> cc
         | otherwise    
         -> error $ unlines
                  [ "DDC.LLVM.Transform.Clean"
                  , "  Not overriding exising calling convention." ]


-- Monads -----------------------------------------------------------------------------------------
type CleanM a = CheckM Int String a


{-
-- | Unique name generation.
newUnique :: CleanM Int
newUnique 
 = do   s       <- get
        put     $ s + 1
        return  $ s


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> CleanM Var
newUniqueVar t
 = do   u <- newUnique
        return $ Var (NameLocal ("_c" ++ show u)) t
-}
