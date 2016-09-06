
-- | Flatten out the extended operators in our expression type to instructions
--   that the LLVM compiler will accept directly.
--
--   The LLVM expresion language is anemic by design. During code generation
--   we use a fatter language, but now need to flatten out the extra operators
--   into plain LLVM instructions.
--
--   This transform is kept separate from the 'Simpl' as it the input and
--   output programs are in different (sub) languages.
--
module DDC.Llvm.Transform.Flatten
        (flatten)
where
import DDC.Llvm.Syntax
import DDC.Control.Check
import Data.Sequence            (Seq, (|>), (><))
import Control.Monad
import qualified Data.Sequence  as Seq
import qualified Data.Foldable  as Seq


-- | Flatten expressions in a module.
flatten :: Module -> Module
flatten mm
 = let  Right funcs'    
                = evalCheck 0 
                $ mapM flattenFunction $ modFuncs mm
   in   mm { modFuncs = funcs' }


-- | Flatten expressions in a function.
flattenFunction :: Function -> FlattenM Function
flattenFunction fun
 = do   blocks' <- mapM flattenBlock $ funBlocks fun
        return  $ fun { funBlocks = blocks' }


-- | Flatten expressions in a single block.
flattenBlock    :: Block   -> FlattenM Block
flattenBlock block
 = do   instrs' <- flattenInstrs Seq.empty 
                $  Seq.toList $ blockInstrs block
        return  $ block { blockInstrs = Seq.fromList instrs' }


-- | Flatten a list of instructions.
flattenInstrs   
        :: Seq AnnotInstr       -- ^ Accumulated instructions of result.
        -> [AnnotInstr]         -- ^ Instructions still to flatten.
        -> FlattenM [AnnotInstr]

flattenInstrs acc [] 
 = return $ Seq.toList acc

flattenInstrs acc (AnnotInstr i annots : is)
 = let  
        next acc'
         = flattenInstrs acc' is

        reannot i'
         = annotWith i' annots

   in case i of

         -- Comments
         IComment{}
          ->    next $ acc |> reannot i

         -- Set meta-instructions.
         ISet v x1
          -> do (is1, x1')     <- flattenX x1
                next $ (acc >< is1) |> (reannot $ ISet v x1')

         -- Preserve nops, for the sake of just doing one thing at a time.
         -- These can be eliminated with the LLVM simplifier.
         INop 
          ->    next $ acc |> reannot i

         -- Phi nodes
         IPhi{}
          ->    next $ acc |> reannot i

         -- Terminator instructions
         IReturn{}
          ->    next $ acc |> reannot i

         IBranch{}
          ->    next $ acc |> reannot i

         IBranchIf x1 l1 l2
          -> do (is1, x1')      <- flattenX x1
                next $ (acc >< is1)  |> (reannot $ IBranchIf x1' l1 l2)

         ISwitch   x1 def alts
          -> do (is1, x1')      <- flattenX x1
                next $ (acc >< is1)  |> (reannot $ ISwitch x1' def alts)

         IUnreachable
          ->    next (acc |> (reannot i))

         -- Operators
         IOp v op x1 x2
          -> do (is1, x1')      <- flattenX x1
                (is2, x2')      <- flattenX x2
                next $ (acc >< is1 >< is2) |> (reannot $ IOp v op x1' x2')

         -- Conversions
         IConv v c x1
          -> do (is1, x1')      <- flattenX x1
                next $ (acc >< is1)  |> (reannot $ IConv v c x1')

         -- Get pointer
         IGet  v x1 os
          -> do (is1, x1')      <- flattenX x1
                next $ (acc >< is1)  |> (reannot $ IGet  v x1' os)

         -- Memory access
         ILoad  v x1
          -> do (is1, x1')      <- flattenX x1
                next $ (acc >< is1)  |> (reannot $ ILoad  v x1')

         IStore x1 x2
          -> do (is1, x1')      <- flattenX x1
                (is2, x2')      <- flattenX x2
                next $ (acc >< is1 >< is2) |> (reannot $ IStore x1' x2')

         -- Comparisons
         ICmp v c x1 x2
          -> do (is1, x1')      <- flattenX x1
                (is2, x2')      <- flattenX x2
                next $ (acc >< is1 >< is2) |> (reannot $ ICmp v c x1' x2')

         -- Function calls
         ICall mv ct mcc t n xs ats        
          -> do (iss, xs')      <- fmap unzip $ mapM flattenX xs
                let is'         =  join $ Seq.fromList iss
                next $  (acc >< is') 
                     |> (reannot $ ICall mv ct mcc t n xs' ats)


---------------------------------------------------------------------------------------------------
-- | Given an extended LLVM expression, strip off our extended XConv and XGet
--   operators and turn them into new instructions. The LLVM compiler itself
--   doesn't accept XConv or XGet in an expression position.
flattenX :: Exp -> FlattenM (Seq AnnotInstr, Exp)
flattenX xx
 = case xx of
        XConv t c x
         -> do  (is', x') <- flattenX x
                v         <- newUniqueVar t
                return    (is' |> (annotNil $ IConv v c x'), XVar v)

        XGet  t x os
         -> do  (is', x') <- flattenX x
                v         <- newUniqueVar t
                return    (is' |> (annotNil $ IGet v x' os), XVar v)

        _ ->    return (Seq.empty, xx)



-- teh monads -------------------------------------------------------------------------------------
type FlattenM a = CheckM Int String a


-- | Unique name generation.
newUnique :: FlattenM Int
newUnique 
 = do   s       <- get
        put     $ s + 1
        return  $ s


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> FlattenM Var
newUniqueVar t
 = do   u <- newUnique
        return $ Var (NameLocal ("_c" ++ show u)) t

