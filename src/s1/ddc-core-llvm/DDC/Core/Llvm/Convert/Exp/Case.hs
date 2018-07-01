
module DDC.Core.Llvm.Convert.Exp.Case
        (convertCase)
where
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Data.ListUtils
import Control.Monad
import Data.Maybe
import Data.Sequence                    (Seq, (<|), (|>), (><))
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Exp           as C
import qualified Data.Sequence          as Seq


-- Case -------------------------------------------------------------------------------------------
convertCase
        :: Context              -- ^ Context of the conversion.
        -> ExpContext           -- ^ Expression context.
        -> Label                -- ^ Label of current block
        -> Seq AnnotInstr       -- ^ Instructions to prepend to initial block.
        -> A.Exp                -- ^ Scrutinee of case expression.
        -> [A.Alt]              -- ^ Alternatives of case expression.
        -> ConvertM (Seq Block)

convertCase ctx ectx label instrs xScrut alts
 | Just mVar    <- takeLocalV ctx xScrut
 = do
        vScrut' <- mVar

        -- Convert all the alternatives.
        -- If we're in a nested context we'll also get a block to join the
        -- results of each alternative.
        (alts', blocksJoin)
         <- convertAlts ctx ectx alts

        -- Determine what default alternative to use for the instruction.
        (lDefault, blocksDefault)
         <- case last alts' of
                AltDefault l bs -> return (l, bs)
                AltCase _  l bs -> return (l, bs)

        -- Get the alternatives before the default one.
        -- This will fail if there are no alternatives at all.
        altsTable
         <- case takeInit alts' of
                Nothing -> throw $ ErrorInvalidExp (A.XCase xScrut alts) Nothing
                Just as -> return as

        -- Build the jump table of non-default alts.
        let table       = mapMaybe takeAltCase altsTable
        let blocksTable = join $ fmap altResultBlocks $ Seq.fromList altsTable

        let switchBlock
                =  Block label
                $  instrs
                |> (annotNil $ ISwitch (XVar vScrut') lDefault table)

        return  $  switchBlock
                <| (blocksTable >< blocksDefault >< blocksJoin)

 | otherwise
 = throw $ ErrorInvalidExp (A.XCase xScrut alts) Nothing


-- Alts -------------------------------------------------------------------------------------------
-- | Convert some case alternatives to LLVM.
convertAlts
        :: Context -> ExpContext
        -> [A.Alt]
        -> ConvertM ([AltResult], Seq Block)

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
        -- Label of the block that does the join.
        lJoin     <- newUniqueLabel "join"

        -- Convert all the alternatives,
        -- assiging their results into separate vars.
        (vDstAlts, alts'@(_:_))
                <- liftM unzip
                $  mapM (\alt -> do
                        vDst'   <- newUniqueNamedVar "alt" (typeOfVar vDst)
                        alt'    <- convertAlt ctx (ExpNest ectx vDst' lJoin) alt
                        lAlt    <- return (altResultLabel alt')
                        return ((XVar vDst', lAlt), alt'))
                $  alts

        -- A block to join the result from each alternative.
        let blockJoin
                = Block lJoin
                $ Seq.fromList $ map annotNil
                [ IPhi vDst vDstAlts
                , IBranch lCont ]

        return (alts', Seq.singleton blockJoin)

-- Cannot convert alternative in this context.
convertAlts _ ExpAssign{} alts
 = throw $ ErrorInvalidAlt alts
         $ Just "Cannot convert alternative in this context."


-- Alt --------------------------------------------------------------------------------------------
-- | Convert a case alternative to LLVM.
--
--   This only works for zero-arity constructors.
--   The client should extract the fields of algebraic data objects manually.
convertAlt
        :: Context -> ExpContext
        -> A.Alt
        -> ConvertM AltResult

convertAlt ctx ectx aa
 = let  pp              = contextPlatform ctx
        convBodyM       = contextConvertBody ctx
   in case aa of
        A.AAlt A.PDefault x
         -> do  label   <- newUniqueLabel "default"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

        A.AAlt (A.PData C.DaConUnit []) x
         -> do  label   <- newUniqueLabel "alt"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

        A.AAlt (A.PData (C.DaConPrim n) []) x
         | Just lit     <- convPatName pp n
         -> do  label   <- newUniqueLabel "alt"
                blocks  <- convBodyM ctx ectx Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

        A.AAlt (A.PData dc@(C.DaConBound{}) _) _x
         ->  throw $ ErrorInvalidAlt [aa] (Just $ "found DaConBound" ++ show dc)

        A.AAlt pat _x
         -> throw $ ErrorInvalidAlt [aa] (Just $ show pat)


-- | Convert a constructor name from a pattern to a LLVM literal.
--
--   Only integral-ish types can be used as patterns, for others
--   such as Floats we rely on the Lite transform to have expanded
--   cases on float literals into a sequence of boolean checks.
convPatName :: Platform -> A.Name -> Maybe Lit
convPatName pp (A.NamePrimLit lit)
 = case lit of
        A.PrimLitBool True
         -> Just $ LitInt (TInt 1) 1

        A.PrimLitBool False
         -> Just $ LitInt (TInt 1) 0

        A.PrimLitNat  i
         -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.PrimLitInt  i
         -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.PrimLitWord i bits
         | elem bits [8, 16, 32, 64]
         -> Just $ LitInt (TInt $ fromIntegral bits) i

        A.PrimLitTag  i
         -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i

        _ -> Nothing

convPatName _ _
 = Nothing


-- | Take the label from an `AltResult`.
altResultLabel :: AltResult -> Label
altResultLabel aa
 = case aa of
        AltDefault label _      -> label
        AltCase  _ label _      -> label


-- | Take the blocks from an `AltResult`.
altResultBlocks :: AltResult -> Seq Block
altResultBlocks aa
 = case aa of
        AltDefault _ blocks     -> blocks
        AltCase _ _  blocks     -> blocks


-- | Take the `Lit` and `Label` from an `AltResult`
takeAltCase :: AltResult -> Maybe (Lit, Label)
takeAltCase ac
 = case ac of
        AltCase lit label _     -> Just (lit, label)
        _                       -> Nothing

