
module DDC.Core.Llvm.Convert.Exp.PrimCast
        (convPrimCast)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Exp.Base
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Base.Pretty
import Data.Sequence                    (Seq)
import qualified DDC.Core.Exp           as C
import qualified DDC.Core.Salt          as A
import qualified Data.Sequence          as Seq
import qualified Data.Map               as Map


-- | Convert a primitive call to LLVM.
convPrimCast
        :: Show a
        => Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Primitive to call.
        -> C.Type A.Name        -- ^ Type of the primitive.
        -> [C.Exp a A.Name]     -- ^ Arguments to primitive.
        -> Maybe (LlvmM (Seq AnnotInstr))

convPrimCast ctx mdst p _tPrim xs
 = let  atom    = mconvAtom       ctx
   in case p of
        A.PrimCast A.PrimCastConvert
         | [C.XType _ tDst, C.XType _ tSrc, xSrc] <- xs
         , Just vDst            <- mdst
         , minstr               <- convPrimConvert ctx tDst vDst tSrc xSrc
         -> Just
          $ case minstr of
                Just instr      -> return $ Seq.singleton (annotNil instr)
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid conversion."
                                , text "  from type: " <> ppr tSrc
                                , text "    to type: " <> ppr tDst ]

        A.PrimCast A.PrimCastPromote
         | [C.XType _ tDst, C.XType _ tSrc, xSrc] <- xs
         , Just xSrc'           <- atom xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimPromote ctx tDst vDst tSrc xSrc'
         -> Just 
          $ case minstr of
                Just instr      -> return $ Seq.singleton (annotNil instr)
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid promotion."
                                , text "  from type: " <> ppr tSrc
                                , text "    to type: " <> ppr tDst ]

        A.PrimCast A.PrimCastTruncate
         | [C.XType _ tDst, C.XType _ tSrc, xSrc] <- xs
         , Just xSrc'           <- atom xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimTruncate ctx tDst vDst tSrc xSrc'
         -> Just 
          $ case minstr of
                Just instr      -> return $ Seq.singleton (annotNil instr)
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid truncation."
                                , text " from type: " <> ppr tSrc
                                , text "   to type: " <> ppr tDst ]

        _ -> Nothing


-- | Convert a primitive conversion operator to LLVM,
--   or `Nothing` for an invalid conversion.
convPrimConvert
        :: Context
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> C.Exp a A.Name
        -> Maybe Instr

convPrimConvert ctx tDst vDst tSrc xSrc
 = let  pp      = contextPlatform ctx
        kenv    = contextKindEnv  ctx
        tSrc'   = convertType pp kenv tSrc
        tDst'   = convertType pp kenv tDst
   in case tSrc' of

        -- Produce the code pointer for a top-level super.
        TPointer TFunction{}
         -- Argument is the name of the super itself.
         | tDst'      == TInt (8 * platformAddrBytes pp)
         , Just xSrc' <- mconvAtom ctx xSrc
         -> Just $ IConv vDst ConvPtrtoint xSrc'

         -- Argument is a variable that has been bound to an application of
         -- a super variable to some type arguments.
         | tDst'      == TInt (8 * platformAddrBytes pp)
         , C.XVar a (C.UName nVar) <- xSrc
         , Just (nSuper, _tsArgs)  <- Map.lookup nVar (contextSuperBinds ctx)
         , Just xSrc' <- mconvAtom ctx (C.XVar a (C.UName nSuper))
         -> Just $ IConv vDst ConvPtrtoint xSrc'

        _ -> Nothing


-- | Convert a primitive promotion operator to LLVM,
--   or `Nothing` for an invalid promotion.
convPrimPromote
        :: Context
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> Exp
        -> Maybe Instr

convPrimPromote ctx tDst vDst tSrc xSrc
 | pp    <- contextPlatform ctx
 , kenv  <- contextKindEnv  ctx
 , tSrc' <- convertType pp kenv tSrc
 , tDst' <- convertType pp kenv tDst
 , Just (A.NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
 , Just (A.NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst
 , A.primCastPromoteIsValid pp tcSrc tcDst
 = case (tDst', tSrc') of
        (TInt bitsDst, TInt bitsSrc)

         -- Same sized integers
         | bitsDst == bitsSrc
         -> Just $ ISet vDst xSrc

         -- Both Unsigned
         | isUnsignedT tSrc, isUnsignedT tDst
         , bitsDst > bitsSrc
         -> Just $ IConv vDst ConvZext xSrc

         -- Both Signed
         | isSignedT tSrc,   isSignedT tDst
         , bitsDst > bitsSrc
         -> Just $ IConv vDst ConvSext xSrc

         -- Unsigned to Signed
         | isUnsignedT tSrc, isSignedT   tDst
         , bitsDst > bitsSrc
         -> Just $ IConv vDst ConvZext xSrc

        _ -> Nothing

 | otherwise = Nothing


-- | Convert a primitive truncation to LLVM,
--   or `Nothing` for an invalid truncation.
convPrimTruncate
        :: Context
        -> C.Type  A.Name -> Var
        -> C.Type  A.Name -> Exp
        -> Maybe Instr

convPrimTruncate ctx tDst vDst tSrc xSrc
 | pp    <- contextPlatform ctx
 , kenv  <- contextKindEnv  ctx
 , tSrc' <- convertType pp kenv tSrc
 , tDst' <- convertType pp kenv tDst
 , Just (A.NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
 , Just (A.NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst
 , A.primCastTruncateIsValid pp tcSrc tcDst
 = case (tDst', tSrc') of
        (TInt bitsDst, TInt bitsSrc)
         -- Same sized integers
         | bitsDst == bitsSrc
         -> Just $ ISet vDst xSrc

         -- Destination is smaller
         | bitsDst < bitsSrc
         -> Just $ IConv vDst ConvTrunc xSrc

         -- Unsigned to Signed,
         --  destination is larger
         | bitsDst > bitsSrc
         , isUnsignedT tSrc,   isSignedT tDst
         -> Just $ IConv vDst ConvZext xSrc

        _ -> Nothing

 | otherwise = Nothing


