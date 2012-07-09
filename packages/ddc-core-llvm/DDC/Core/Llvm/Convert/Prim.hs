
module DDC.Core.Llvm.Convert.Prim
        (convPrimCallM)
where
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Type.Compounds
import DDC.Base.Pretty
import Data.Sequence            (Seq)
import qualified DDC.Core.Exp   as C
import qualified DDC.Core.Salt  as A
import qualified Data.Sequence  as Seq


-- Prim call ------------------------------------------------------------------
-- | Convert a primitive call to LLVM.
convPrimCallM 
        :: Show a 
        => Platform             -- ^ Current platform.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.Prim               -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> LlvmM (Seq Instr)

convPrimCallM pp mdst p tPrim xs
 = case p of
        -- Binary operations ----------
        A.PrimOp op
         | C.XType t : args     <- xs
         , Just [x1', x2']      <- mconvAtoms pp args
         , Just dst             <- mdst
         -> let result
                 | Just op'     <- convPrimOp2 op t
                 = IOp dst op' x1' x2'

                 | Just icond'  <- convPrimICond op t
                 = IICmp dst icond' x1' x2'

                 | Just fcond'  <- convPrimFCond op t
                 = IFCmp dst fcond' x1' x2'

                 | otherwise
                 = die "Invalid binary primop."
           in   return $ Seq.singleton result

        -- Cast primops ---------------
        A.PrimCast A.PrimCastPromote
         | [C.XType tDst, C.XType tSrc, xSrc] <- xs
         , Just xSrc'           <- mconvAtom pp xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimPromote pp tDst vDst tSrc xSrc'
         -> case minstr of
                Just instr      -> return $ Seq.singleton instr
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid promotion of numeric value."
                                , text "  from type: " <> ppr tSrc
                                , text "    to type: " <> ppr tDst]

        A.PrimCast A.PrimCastTruncate
         | [C.XType tDst, C.XType tSrc, xSrc] <- xs
         , Just xSrc'           <- mconvAtom pp xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimTruncate pp tDst vDst tSrc xSrc'
         -> case minstr of
                Just instr      -> return $ Seq.singleton instr
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid truncation of numeric value."
                                , text " from type: " <> ppr tSrc
                                , text "   to type: " <> ppr tDst ]

        -- Store primops --------------
        A.PrimStore A.PrimStoreAlloc
         | Just [xBytes']       <- mconvAtoms pp xs
         -> return      $ Seq.singleton
                        $ ICall mdst CallTypeStd
                                (tAddr pp) (NameGlobal "malloc") 
                                [xBytes'] []

        A.PrimStore A.PrimStoreRead
         | C.XType _t : args             <- xs
         , Just [xAddr', xOffset']      <- mconvAtoms pp args
         , Just vDst@(Var nDst tDst)    <- mdst
         -> let vOff    = Var (bumpName nDst "off") (tAddr pp)
                vPtr    = Var (bumpName nDst "ptr") (tPtr tDst)
            in  return  $ Seq.fromList
                        [ IOp   vOff OpAdd xAddr' xOffset'
                        , IConv vPtr ConvInttoptr (XVar vOff)
                        , ILoad vDst (XVar vPtr) ]

        A.PrimStore A.PrimStoreWrite
         | C.XType _t : args              <- xs
         , Just [xAddr', xOffset', xVal'] <- mconvAtoms pp args      
         -> do  vOff    <- newUniqueNamedVar "off" (tAddr pp)
                vPtr    <- newUniqueNamedVar "ptr" (tPtr $ typeOfExp xVal')
                return  $ Seq.fromList
                        [ IOp    vOff OpAdd xAddr' xOffset'
                        , IConv  vPtr ConvInttoptr (XVar vOff)
                        , IStore (XVar vPtr) xVal' ]

        A.PrimStore A.PrimStorePlusAddr
         | Just [xAddr', xOffset']      <- mconvAtoms pp xs
         , Just vDst                    <- mdst
         ->     return  $ Seq.singleton
                        $ IOp vDst OpAdd xAddr' xOffset'

        A.PrimStore A.PrimStoreMinusAddr
         | Just [xAddr', xOffset']      <- mconvAtoms pp xs
         , Just vDst                    <- mdst
         ->     return  $ Seq.singleton
                        $ IOp vDst OpSub xAddr' xOffset'

        A.PrimStore A.PrimStorePeek
         | C.XType tDst : args          <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp args
         , Just vDst@(Var nDst _)       <- mdst
         , tDst'                        <- convType   pp tDst
         -> let vAddr1   = Var (bumpName nDst "addr1") (tAddr pp)
                vAddr2   = Var (bumpName nDst "addr2") (tAddr pp)
                vPtr     = Var (bumpName nDst "ptr")   (tPtr tDst')
            in  return  $ Seq.fromList
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2)
                        , ILoad vDst  (XVar vPtr) ]

        A.PrimStore A.PrimStorePoke
         | C.XType tDst : args           <- xs
         , Just [xPtr', xOffset', xVal'] <- mconvAtoms pp args
         , tDst'                         <- convType pp tDst
         -> do  vAddr1  <- newUniqueNamedVar "addr1" (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2" (tAddr pp)
                vPtr    <- newUniqueNamedVar "ptr"   (tPtr tDst')
                return  $ Seq.fromList
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2)
                        , IStore (XVar vPtr) xVal' ]

        A.PrimStore A.PrimStorePlusPtr
         | _xType : args                <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp args
         , Just vDst                    <- mdst
         -> do  vAddr   <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]

        A.PrimStore A.PrimStoreMinusPtr
         | _xType : args                <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp args
         , Just vDst                    <- mdst
         -> do  vAddr   <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpSub (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]

        A.PrimStore A.PrimStoreMakePtr
         | [C.XType _t, xAddr]          <- xs
         , Just xAddr'  <- mconvAtom pp xAddr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton
                        $ IConv vDst ConvInttoptr xAddr'

        A.PrimStore A.PrimStoreTakePtr
         | [C.XType _t, xPtr]          <- xs
         , Just xPtr'   <- mconvAtom pp xPtr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton
                        $ IConv vDst ConvPtrtoint xPtr'

        A.PrimStore A.PrimStoreCastPtr
         | [C.XType _tSrc, C.XType _tDst, xPtr] <- xs
         , Just xPtr'   <- mconvAtom pp xPtr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton
                        $ IConv vDst ConvBitcast xPtr'

        -- External Primops -----------
        A.PrimExternal prim
         |  Just xs'     <- sequence $ map (mconvAtom pp) xs
         ,  (_, tResult) <- takeTFunArgResult tPrim
         ,  tResult'     <- convType pp tResult
         ,  Just name'   <- convPrimExtern prim tPrim
         -> return      $ Seq.singleton
                        $ ICall mdst CallTypeStd tResult'
                                name' xs' []

        _ -> die "Invalid prim call."



bumpName :: Name -> String -> Name
bumpName nn s
 = case nn of
        NameLocal str   -> NameLocal  (str ++ "." ++ s)
        NameGlobal str  -> NameGlobal (str ++ "." ++ s)


-- Op -------------------------------------------------------------------------
-- | Convert a binary primop from Core Sea to LLVM form.
convPrimOp2 :: A.PrimOp -> C.Type A.Name -> Maybe Op
convPrimOp2 op t
 = case op of
        A.PrimOpAdd     
         | isIntegralT t                -> Just OpAdd
         | isFloatingT t                -> Just OpFAdd 

        A.PrimOpSub      
         | isIntegralT t                -> Just OpSub
         | isFloatingT t                -> Just OpFSub

        A.PrimOpMul 
         | isIntegralT t                -> Just OpMul
         | isFloatingT t                -> Just OpFMul

        A.PrimOpDiv
         | isIntegralT t, isUnsignedT t -> Just OpUDiv
         | isIntegralT t, isSignedT t   -> Just OpSDiv
         | isFloatingT t                -> Just OpFDiv

        A.PrimOpRem
         | isIntegralT t, isUnsignedT t -> Just OpURem
         | isIntegralT t, isSignedT t   -> Just OpSRem
         | isFloatingT t                -> Just OpFRem

        A.PrimOpShl
         | isIntegralT t                -> Just OpShl

        A.PrimOpShr
         | isIntegralT t, isUnsignedT t -> Just OpLShr
         | isIntegralT t, isSignedT t   -> Just OpAShr

        A.PrimOpBAnd
         | isIntegralT t                -> Just OpAnd

        A.PrimOpBOr
         | isIntegralT t                -> Just OpOr

        A.PrimOpBXOr
         | isIntegralT t                -> Just OpXor

        _                               -> Nothing


-- Cast -----------------------------------------------------------------------

-- | Convert a primitive promotion to LLVM.
convPrimPromote 
        :: Platform 
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> Exp
        -> Maybe Instr

convPrimPromote pp tDst vDst tSrc xSrc
 = let  tDst'   = convType pp tDst
        tSrc'   = convType pp tSrc
   in case (tDst', tSrc') of
        (TInt bitsDst, TInt bitsSrc)
         -- Same sized integers
         | bitsDst == bitsSrc       
         -> Just $ ISet vDst xSrc

         -- Both Unsigned
         | isUnsignedT tSrc
         , isUnsignedT tDst
         , bitsDst > bitsSrc        
         -> Just $ IConv vDst ConvZext xSrc

         -- Both Signed
         | isSignedT tSrc
         , isSignedT tDst
         , bitsDst > bitsSrc
         -> Just $ IConv vDst ConvSext xSrc

         -- Unsigned to Signed
         | isUnsignedT tSrc
         , isSignedT   tDst
         , bitsDst > bitsSrc
         -> Just $ IConv vDst ConvZext xSrc

        _ -> Nothing


-- | Convert a primitive truncation to LLVM.
convPrimTruncate
        :: Platform 
        -> C.Type A.Name -> Var
        -> C.Type A.Name -> Exp
        -> Maybe Instr

convPrimTruncate pp tDst vDst tSrc xSrc
 = let  tDst'   = convType pp tDst
        tSrc'   = convType pp tSrc
   in case (tDst', tSrc') of
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
         , isUnsignedT tSrc
         , isSignedT   tDst
         -> Just $ IConv vDst ConvZext xSrc

        _ -> Nothing


-- Cond -----------------------------------------------------------------------
-- | Convert an integer comparison from Core Sea to LLVM form.
convPrimICond :: A.PrimOp -> C.Type A.Name -> Maybe ICond
convPrimICond op t
 | isIntegralT t
 = case op of
        A.PrimOpEq      -> Just ICondEq
        A.PrimOpNeq     -> Just ICondNe
        A.PrimOpGt      -> Just ICondUgt
        A.PrimOpGe      -> Just ICondUge
        A.PrimOpLt      -> Just ICondUlt
        A.PrimOpLe      -> Just ICondUle
        _               -> Nothing

 | otherwise            =  Nothing


-- | Convert a floating point comparison from Core Sea to LLVM form.
convPrimFCond :: A.PrimOp -> C.Type A.Name -> Maybe FCond
convPrimFCond op t
 | isIntegralT t
 = case op of
        A.PrimOpEq      -> Just FCondOeq
        A.PrimOpNeq     -> Just FCondOne
        A.PrimOpGt      -> Just FCondOgt
        A.PrimOpGe      -> Just FCondOge
        A.PrimOpLt      -> Just FCondOlt
        A.PrimOpLe      -> Just FCondOle
        _               -> Nothing

 | otherwise            =  Nothing


-- Extern ---------------------------------------------------------------------
-- | Get the symbol name of an external primitive.
convPrimExtern :: A.PrimExternal -> C.Type A.Name -> Maybe Name
convPrimExtern p _t
 = case p of
        A.PrimExternalShowInt
         -> Just $ NameGlobal "showInt"

        A.PrimExternalPutStr
         -> Just $ NameGlobal "putStr"

        A.PrimExternalPutStrLn
         -> Just $ NameGlobal "putStrLn"


