
module DDC.Core.Llvm.Convert.Exp.Case
        (convertCase)
where
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Exp.Base
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Data.ListUtils
import Control.Monad
import Data.Maybe
import Data.Sequence                            (Seq, (<|), (|>), (><))
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Exp                   as C
import qualified Data.Sequence                  as Seq


-- Case -------------------------------------------------------------------------------------------
convertCase
        :: Context              -- ^ Context of the conversion.
        -> ExpContext           -- ^ Expression context.
        -> Label                -- ^ Label of current block
        -> Seq AnnotInstr       -- ^ Instructions to prepend to initial block.
        -> C.Exp () A.Name      -- ^ Scrutinee of case expression.
        -> [C.Alt () A.Name]    -- ^ Alternatives of case expression.
        -> LlvmM (Seq Block)

convertCase ctx ectx label instrs xScrut alts 
 = let  pp              = contextPlatform ctx
        kenv            = contextKindEnv  ctx
        tenv            = contextTypeEnv  ctx
   in 
    case takeLocalV pp kenv tenv xScrut of
     Just vScrut'@Var{} -> do
        -- Convert all the alternatives.
        -- If we're in a nested context we'll also get a block to join the 
        -- results of each alternative.
        (alts', blocksJoin) <- convertAlts ctx ectx alts

        -- Build the switch ---------------
        -- Determine what default alternative to use for the instruction. 
        (lDefault, blocksDefault)
         <- case last alts' of
                AltDefault l bs -> return (l, bs)
                AltCase _  l bs -> return (l, bs)

        -- Alts that aren't the default.
        let Just altsTable = takeInit alts'

        -- Build the jump table of non-default alts.
        let table       = mapMaybe takeAltCase altsTable
        let blocksTable = join $ fmap altResultBlocks $ Seq.fromList altsTable

        let switchBlock 
                =  Block label
                $  instrs 
                |> (annotNil $ ISwitch (XVar vScrut') lDefault table)

        return  $  switchBlock 
                <| (blocksTable >< blocksDefault >< blocksJoin)

     Nothing 
      -> die "Invalid case expression"


-- Alts -------------------------------------------------------------------------------------------
convertAlts
        :: Context -> ExpContext
        -> [C.Alt () A.Name]
        -> LlvmM ([AltResult], Seq Block)

-- Alternatives are at top level.
convertAlts ctx ectx@ExpTop{} alts
 = do   
        alts'   <- mapM (convertAlt ctx ectx) alts
        return  (alts', Seq.empty)


-- If we're doing a branch inside a let-binding we need to add a join
-- point to collect the results from each altenative before continuing
-- on to evaluate the rest.
convertAlts ctx (ExpNest ectx vDst lCont) alts
 = do
        let tDst'       = typeOfVar vDst

        -- Label of the block that does the join.
        lJoin           <- newUniqueLabel "join"

        -- Convert all the alternatives,
        -- assiging their results into separate vars.
        (vDstAlts, alts'@(_:_))
                <- liftM unzip 
                $  mapM (\alt -> do
                        vDst'   <- newUniqueNamedVar "alt" tDst'
                        alt'    <- convertAlt ctx (ExpNest ectx vDst' lJoin) alt
                        return (vDst', alt'))
                $  alts

        -- A block to join the result from each alternative.
        --  Trying to keep track of which block a variable is defined in is 
        --  too hard when we have nested join points. 
        --  Instead, we set the label here to 'unknown' and fix this up in the
        --  Clean transform.
        let blockJoin   
                = Block lJoin
                $ Seq.fromList $ map annotNil
                [ IPhi vDst  [ (XVar vDstAlt, Label "unknown")
                             | vDstAlt   <- vDstAlts ]
                , IBranch lCont ]

        return (alts', Seq.singleton blockJoin)

convertAlts _ ExpAssign{} _
 = die "Cannot convert alts in this context."


-- Alt --------------------------------------------------------------------------------------------
-- | Convert a case alternative to LLVM.
--
--   This only works for zero-arity constructors.
--   The client should extrac the fields of algebraic data objects manually.
convertAlt
        :: Context
        -> ExpContext           -- ^ Context we're converting in.
        -> C.Alt () A.Name      -- ^ Alternative to convert.
        -> LlvmM AltResult

convertAlt ctx ectx aa
 = let  pp              = contextPlatform ctx
        convBodyM       = contextConvertBody ctx
   in case aa of
        C.AAlt C.PDefault x
         -> do  label   <- newUniqueLabel "default"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

        C.AAlt (C.PData C.DaConUnit []) x
         -> do  label   <- newUniqueLabel "alt"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

        C.AAlt (C.PData dc []) x
         | Just n       <- takeNameOfDaCon dc
         , Just lit     <- convPatName pp n
         -> do  label   <- newUniqueLabel "alt"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

        _ -> die "Invalid alternative"


-- | Convert a constructor name from a pattern to a LLVM literal.
--
--   Only integral-ish types can be used as patterns, for others 
--   such as Floats we rely on the Lite transform to have expanded
--   cases on float literals into a sequence of boolean checks.
convPatName :: Platform -> A.Name -> Maybe Lit
convPatName pp name
 = case name of
        A.NameLitBool True   -> Just $ LitInt (TInt 1) 1
        A.NameLitBool False  -> Just $ LitInt (TInt 1) 0

        A.NameLitNat  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.NameLitInt  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.NameLitWord i bits 
         | elem bits [8, 16, 32, 64]
         -> Just $ LitInt (TInt $ fromIntegral bits) i

        A.NameLitTag  i      -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i

        _                    -> Nothing


-- | Take the blocks from an `AltResult`.
altResultBlocks :: AltResult -> Seq Block
altResultBlocks aa
 = case aa of
        AltDefault _ blocks     -> blocks
        AltCase _ _  blocks     -> blocks


-- | Take the `Lit` and `Label` from an `AltResult`
takeAltCase :: AltResult -> Maybe (Lit, Label)
takeAltCase (AltCase lit label _)       = Just (lit, label)
takeAltCase _                           = Nothing

