
-- | Translate our extended LLVM AST to the form that the LLVM compiler will
--   accept directly.
--
--   * inline `ISet` meta-instructions
--   * drop `INop` meta-instructions
--   * propagate calling conventions from declarations to call sites.
--   * Strip `XConv` and `XGet` expressions into separate instructinos.
--
module DDC.Llvm.Transform.Clean
        (clean)
where
import DDC.Llvm.Syntax
import DDC.Control.Monad.Check
import Data.Map                 (Map)
import Data.Sequence            (Seq, (|>), (><))
import qualified Data.Map       as Map
import qualified Data.Foldable  as Seq
import qualified Data.Sequence  as Seq
import Control.Monad


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
        ISet v x1              
         -> do  (is1, x1')  <- stripX x1
                let binds'  = Map.insert v x1' binds
                next binds' defs (acc >< is1)

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
         -> do  (is1, x1')  <- stripX x
                next binds defs 
                 $  (acc >< is1)        |> (reAnnot $ IReturn (Just (sub x1')))

        IBranch{}
         ->     next binds defs
                 $  acc |> ins

        IBranchIf x l1 l2
         -> do  (is1, x1') <- stripX x
                next binds defs
                 $  (acc >< is1)        |> (reAnnot $ IBranchIf (sub x1') l1 l2)

        ISwitch x def alts
         -> do  (is1, x1') <- stripX x
                next binds defs
                 $  (acc >< is1)        |> (reAnnot $ ISwitch   (sub x1') def alts)

        IUnreachable
         ->     next binds defs
                 $  acc |> ins

        IOp    v op x1 x2
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                (is2, x2')  <- stripX x2
                next binds defs'
                 $ (acc >< is1 >< is2)  |> (reAnnot $ IOp  v op (sub x1') (sub x2'))

        IConv  v c x1
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                next binds defs'
                 $ (acc >< is1)         |> (reAnnot $ IConv v c (sub x1'))

        IGet   v x1 os
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                next binds defs'
                 $ (acc >< is1)         |> (reAnnot $ IGet v    (sub x1') os)

        ILoad  v x1
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                next binds defs'
                 $ (acc >< is1)         |> (reAnnot $ ILoad v   (sub x1'))

        IStore x1 x2
         -> do  (is1, x1')  <- stripX x1
                (is2, x2')  <- stripX x2
                next binds defs
                 $ (acc >< is1 >< is2)  |> (reAnnot $ IStore    (sub x1') (sub x2'))

        IICmp  v c x1 x2
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                (is2, x2')  <- stripX x2
                next binds defs'
                 $ (acc >< is1 >< is2)  |> (reAnnot $ IICmp v c (sub x1') (sub x2'))

        IFCmp  v c x1 x2
         -> do  let defs'   = Map.insert v label defs
                (is1, x1')  <- stripX x1
                (is2, x2')  <- stripX x2
                next binds defs'
                 $ (acc >< is1 >< is2)  |> (reAnnot $ IFCmp v c (sub x1') (sub x2'))

        ICall  (Just v) ct mcc t n xs ats
         -> do  let defs'    =  Map.insert v label defs
                let Just cc2 =  callConvOfName mm n
                let cc'      =  mergeCallConvs mcc cc2
                (iss, xs')   <- liftM unzip $ mapM stripX xs
                let is'      =  join $ Seq.fromList iss

                next binds defs'
                 $  (acc >< is')       
                 |> (reAnnot $ ICall (Just v) ct (Just cc') t n (map sub xs') ats) 

        ICall  Nothing ct mcc t n xs ats
         -> do  let Just cc2 =  callConvOfName mm n
                let cc'      =  mergeCallConvs mcc cc2
                (iss, xs')   <- liftM unzip $ mapM stripX xs
                let is'      =  join $ Seq.fromList iss 
                next binds defs
                 $  (acc >< is') 
                 |> (reAnnot $ ICall Nothing  ct (Just cc') t n (map sub xs') ats) 


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Given an extended LLVM expression, strip off our extended XConv and XGet
--   operators and turn them into new instructions. The LLVM compiler itself
--   doesn't accept XConv or XGet in an expression position.
stripX :: Exp -> CleanM (Seq AnnotInstr, Exp)
stripX xx
 = case xx of
        XConv t c x
         -> do  (is', x') <- stripX x
                v         <- newUniqueVar t
                return    (is' |> (annotNil $ IConv v c x'), XVar v)

        XGet  t x os
         -> do  (is', x') <- stripX x
                v         <- newUniqueVar t
                return    (is' |> (annotNil $ IGet v x' os), XVar v)

        _ ->    return (Seq.empty, xx)


-- Monads -----------------------------------------------------------------------------------------
type CleanM a = CheckM Int String a


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

