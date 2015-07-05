
-- | LLVM program simplifier.
--
--   The LLVM compiler itself already contains a metric crapton of transforms
--   that we don't want to re-implement here. However, these simple things
--   are useful when normalising the code emitted by the code generator, 
--   so that the LLVM compiler will actually accept it.
--   
module DDC.Llvm.Transform.Simpl
        ( simpl
        , Config (..)
        , configZero)
where
import DDC.Llvm.Syntax
import DDC.Llvm.Analysis.Defs
import DDC.Control.Monad.Check
import Data.Sequence            (Seq, (|>))
import Data.Map                 (Map)
import qualified Data.Map       as Map
import qualified Data.Foldable  as Seq
import qualified Data.Sequence  as Seq


---------------------------------------------------------------------------------------------------
-- | Simplifier config.
data Config
        = Config
        { -- | Drop NOP instructions.
          configDropNops         :: Bool

          -- | Inline simple v1 v2 bindings.
        , configSimplAlias      :: Bool

          -- | Inline simple v1 const bindings.
          --
          --   NOTE: Inlining constants into phi nodes before the 'from' labels for
          --         each in-edge are filled will lose information and render the
          --         program uncompilable.
        , configSimplConst      :: Bool }


-- | Config with all transforms disabled.
configZero :: Config
configZero
        = Config
        { configDropNops        = False
        , configSimplAlias      = False
        , configSimplConst      = False }


---------------------------------------------------------------------------------------------------
-- | Simplify a module.
simpl :: Config -> Module -> Module
simpl config mm
 = let  Right funcs'    
                = evalCheck ()
                $ mapM (simplFunction config) $ modFuncs mm
   in   mm { modFuncs = funcs' }


-- | Simplify the body of a function.
simplFunction :: Config -> Function -> SimplM Function
simplFunction config fun
 = do   
        -- Build a map of how all the variables in this function are defined.
        let defs = Map.unions 
                 $ map defsOfBlock $ funBlocks fun

        -- Simplify each block in turn.
        blocks'  <- mapM (simplBlock config defs) 
                 $ funBlocks fun

        return   $ fun { funBlocks = blocks' }


-- | Simplify a single block.
simplBlock    
        :: Config               -- ^ Simplifier configuration.
        -> Map Var (Label, Def) -- ^ How each variable in the function is defined.
        -> Block                -- ^ Block to simplify.
        -> SimplM Block

simplBlock config defs block
 = do   instrs' <- simplInstrs config defs Seq.empty 
                $  Seq.toList $ blockInstrs block
        return  $ block { blockInstrs = Seq.fromList instrs' }


-- | Simplify a list of instructions.
simplInstrs
        :: Config               -- ^ Simplifier configuration.
        -> Map Var (Label, Def) -- ^ How each variable in the function is defined.
        -> Seq AnnotInstr       -- ^ Accumulated instructions of result.
        -> [AnnotInstr]         -- ^ Instructions still to simplify.
        -> SimplM [AnnotInstr]

simplInstrs _config _defs acc []
 = return $ Seq.toList acc

simplInstrs config defs acc (AnnotInstr i annots : is)
 = let
        -- Move to the next instruction in the sequence.
        next acc'
         = simplInstrs config defs acc' is

        -- Attach the annotation back to this instruction.
        reannot i'
         = annotWith i' annots

        -- Use the defs map to try to substitue this variable for 
        -- something even better.
        subst xx0
         = go (0 :: Int) xx0
         where 
                go !n _xx
                 -- Bail out to avoid diverging when there is a loop in the definitions.
                 -- This should never happen in a sane, well formed program.
                 |  n > 1000000
                 = throw ErrorSimplAliasLoop

                go !n xx
                 = case xx of
                        XVar v
                         -> case Map.lookup v defs of
                                Just (_, DefAlias v')
                                 | configSimplAlias config
                                 -> go (n + 1) (XVar v')

                                Just (_, DefClosedConstant xx')
                                 | configSimplConst config
                                 -> return xx'

                                _ -> return xx
                        _ -> return xx

   in case i of

        -- Comments
        IComment{}
         ->     next $ acc |> reannot i

        -- Set meta-instructions.
        ISet v1 x2
         -- Simple aliases being substituted out.
         | XVar _v2     <- x2
         , configSimplAlias config
         ->     next acc

         -- Closed constants being substituted out.
         | isClosedConstantExp x2
         , configSimplConst config
         ->     next acc

         | otherwise
         -> do  x2'     <- subst x2
                next $ acc |> reannot (ISet v1 x2')

        -- Drop nops if we were asked to.
        INop
         | configDropNops config
         ->     next acc

         | otherwise
         ->     next $ acc |> reannot i

        -- Phi nodes.
        IPhi v xls
         -> do  xs'     <- mapM subst $ map fst xls
                let ls' =  map snd xls
                next $ acc |> reannot (IPhi v $ zip xs' ls')

        -- Terminator instructions
        IReturn mx
         -> do  mx'     <- case mx of
                                Nothing -> return Nothing
                                Just x  -> fmap Just $ subst x

                next $ acc |> reannot (IReturn mx')

        IBranch{}
         ->     next $ acc |> reannot i

        IBranchIf x1 l2 l3
         -> do  x1'     <- subst x1
                next $ acc |> reannot (IBranchIf x1' l2 l3)

        ISwitch x1 def alts
         -> do  x1'     <- subst x1
                next $ acc |> reannot (ISwitch   x1' def alts)

        IUnreachable
         ->     next $ acc |> reannot i

        -- Operators
        IOp v op x1 x2
         -> do  x1'     <- subst x1
                x2'     <- subst x2
                next $ acc |> reannot (IOp   v op x1' x2')

        -- Conversions
        IConv v c x1
         -> do  x1'     <- subst x1
                next $ acc |> reannot (IConv v c x1')

        -- Get pointer
        IGet  v x1 os
         -> do  x1'     <- subst x1
                next $ acc |> reannot (IGet  v x1' os)

        -- Memory instructions
        ILoad v x1
         -> do  x1'     <- subst x1
                next $ acc |> reannot (ILoad v x1')

        IStore x1 x2
         -> do  x1'     <- subst x1
                x2'     <- subst x2
                next $ acc |> reannot (IStore x1' x2')

        -- Comparisons
        ICmp v c x1 x2
         -> do  x1'     <- subst x1
                x2'     <- subst x2
                next $ acc |> reannot (ICmp  v c x1' x2')

        -- Calls
        ICall mv cc mcc t n xs ats
         -> do  xs'     <- mapM subst xs
                next $ acc |> reannot (ICall mv cc mcc t n xs' ats)


-- teh monads -------------------------------------------------------------------------------------
type SimplM a = CheckM () ErrorSimpl a


-- | Things that can go wrong during simplification.
data ErrorSimpl
        -- | Substitution for v1 = v2 didn't complete after a sane
        --   number of iterations. There might be a loop in the definitions.
        = ErrorSimplAliasLoop

