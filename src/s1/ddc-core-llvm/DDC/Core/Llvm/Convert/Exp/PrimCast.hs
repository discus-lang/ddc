
module DDC.Core.Llvm.Convert.Exp.PrimCast
        (convPrimCast)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import DDC.Core.Salt.Platform
import Data.Sequence                    (Seq)
import qualified DDC.Core.Exp           as C
import qualified DDC.Core.Salt          as A
import qualified Data.Sequence          as Seq
import qualified Data.Map               as Map


-------------------------------------------------------------------------------
-- | Convert a primitive call to LLVM,
--   or Nothing if this doesn't look like such an operation.
convPrimCast
        :: Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Primitive to call.
        -> [A.Arg]              -- ^ Arguments to primitive.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimCast ctx mdst p as
 = case p of
        A.PrimCast A.PrimCastConvert
         | [A.RType tDst, A.RType tSrc, xSrc] <- as
         , Just vDst            <- mdst
         -> Just $ do
                instr   <- convPrimConvert ctx tDst vDst tSrc xSrc
                return  $  Seq.singleton (annotNil instr)

        A.PrimCast A.PrimCastPromote
         | [A.RType tDst, A.RType tSrc, xSrc] <- as
         , Just vDst            <- mdst
         , Just mSrc            <- mconvArg ctx xSrc
         -> Just $ do
                xSrc'   <- mSrc
                instr   <- convPrimPromote ctx tDst vDst tSrc xSrc'
                return  $  Seq.singleton (annotNil instr)

        A.PrimCast A.PrimCastTruncate
         | [A.RType tDst, A.RType tSrc, xSrc] <- as
         , Just vDst            <- mdst
         , Just mSrc            <- mconvArg ctx xSrc
         -> Just $ do
                xSrc'   <- mSrc
                instr   <- convPrimTruncate ctx tDst vDst tSrc xSrc'
                return  $  Seq.singleton (annotNil instr)

        _ -> Nothing


-------------------------------------------------------------------------------
-- | Convert a primitive conversion operator to LLVM,
--   or `Nothing` for an invalid conversion.
convPrimConvert
        :: Context
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> A.Arg
        -> ConvertM Instr

convPrimConvert ctx tDst vDst tSrc aSrc
 | pp     <- contextPlatform ctx
 , kenv   <- contextKindEnv  ctx
 = do
        tSrc'   <- convertType pp kenv tSrc
        tDst'   <- convertType pp kenv tDst

        case tSrc' of

         -- Produce the code pointer for a top-level super.
         TPointer TFunction{}

          -- Argument is the name of the super itself.
          | tDst'      == TInt (8 * platformAddrBytes pp)
          , Just mSrc               <- mconvArg ctx aSrc
          -> do xSrc' <- mSrc
                return $ IConv vDst ConvPtrtoint xSrc'

          -- Argument is a variable that has been bound to an application of
          -- a super variable to some type arguments.
          | tDst'      == TInt (8 * platformAddrBytes pp)
          , A.RExp (A.XVar (C.UName nVar))   <- aSrc
          , Just (nSuper, _tsArgs)  <- Map.lookup nVar (contextSuperBinds ctx)
          , Just mSrc               <- mconvArg ctx (A.RExp (A.XVar (C.UName nSuper)))
          -> do xSrc' <- mSrc
                return $ IConv vDst ConvPtrtoint xSrc'

         -- Conversion is not valid on this platform.
         _ -> throw $ ErrorInvalidConversion tSrc tDst


-------------------------------------------------------------------------------
-- | Convert a primitive promotion operator to LLVM,
--   or `Nothing` for an invalid promotion.
convPrimPromote
        :: Context
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> Exp
        -> ConvertM Instr

convPrimPromote ctx tDst vDst tSrc xSrc
 = do
        let pp   =  contextPlatform ctx
        let kenv =  contextKindEnv  ctx

        tSrc'    <- convertType pp kenv tSrc
        tDst'    <- convertType pp kenv tDst

        case (tDst', tSrc') of
         (TInt bitsDst, TInt bitsSrc)

          -- Same sized integers
          | bitsDst == bitsSrc
          -> return $ ISet vDst xSrc

          -- Both Unsigned
          | isUnsignedT tSrc, isUnsignedT tDst
          , bitsDst > bitsSrc
          -> return $ IConv vDst ConvZext xSrc

          -- Both Signed
          | isSignedT tSrc,   isSignedT tDst
          , bitsDst > bitsSrc
          -> return $ IConv vDst ConvSext xSrc

          -- Unsigned to Signed
          | isUnsignedT tSrc, isSignedT   tDst
          , bitsDst > bitsSrc
          -> return $ IConv vDst ConvZext xSrc

         (TFloat,  TInt bitsSrc)
          | bitsSrc > 23        -> throw  $ ErrorInvalidPromotion  tSrc tDst
          | isUnsignedT tSrc    -> return $ IConv vDst ConvUintofp xSrc
          | isSignedT   tSrc    -> return $ IConv vDst ConvSintofp xSrc
          | otherwise           -> throw  $ ErrorInvalidPromotion  tSrc tDst

         (TDouble, TInt bitsSrc)
          | bitsSrc > 52        -> throw  $ ErrorInvalidPromotion  tSrc tDst
          | isUnsignedT tSrc    -> return $ IConv vDst ConvUintofp xSrc
          | isSignedT   tSrc    -> return $ IConv vDst ConvSintofp xSrc
          | otherwise           -> throw  $ ErrorInvalidPromotion  tSrc tDst

         -- Promotion is not valid on this platform.
         _ -> throw $ ErrorInvalidPromotion tSrc tDst


-------------------------------------------------------------------------------
-- | Convert a primitive truncation to LLVM,
--   or `Nothing` for an invalid truncation.
convPrimTruncate
        :: Context
        -> C.Type  A.Name -> Var
        -> C.Type  A.Name -> Exp
        -> ConvertM Instr

convPrimTruncate ctx tDst vDst tSrc xSrc
 = do
        let pp   = contextPlatform ctx
        let kenv = contextKindEnv  ctx

        tSrc' <- convertType pp kenv tSrc
        tDst' <- convertType pp kenv tDst

        case (tDst', tSrc') of
         (TInt bitsDst, TInt bitsSrc)
          -- Same sized integers
          | bitsDst == bitsSrc
          -> return $ ISet vDst xSrc

          -- Destination is smaller
          | bitsDst < bitsSrc
          -> return $ IConv vDst ConvTrunc xSrc

          -- Unsigned to Signed,
          --  destination is larger
          | bitsDst > bitsSrc
          , isUnsignedT tSrc,   isSignedT tDst
          -> return $ IConv vDst ConvZext xSrc

         (TFloat, TInt bitsSrc)
          | bitsSrc <= 23       -> throw  $ ErrorInvalidTruncation tSrc tDst
          | isUnsignedT tSrc    -> return $ IConv vDst ConvUintofp xSrc
          | isSignedT   tSrc    -> return $ IConv vDst ConvSintofp xSrc
          | otherwise           -> throw  $ ErrorInvalidTruncation tSrc tDst

         (TDouble, TInt bitsSrc)
          | bitsSrc <= 52       -> throw  $ ErrorInvalidTruncation tSrc tDst
          | isUnsignedT tSrc    -> return $ IConv vDst ConvUintofp xSrc
          | isSignedT   tSrc    -> return $ IConv vDst ConvSintofp xSrc
          | otherwise           -> throw  $ ErrorInvalidTruncation tSrc tDst

         -- Truncation is not valid on this platform.
         _ -> error $ show (tDst', tSrc') -- throw $ ErrorInvalidTruncation tSrc tDst

