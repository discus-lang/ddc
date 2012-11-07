
module DDC.Core.Llvm.Convert.Prim
        (convPrimCallM)
where
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Metadata.Tbaa
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Base.Pretty
import DDC.Type.Env             (KindEnv, TypeEnv)
import Data.Sequence            (Seq)
import qualified DDC.Core.Exp   as C
import qualified DDC.Core.Salt  as A
import qualified Data.Sequence  as Seq


-- Prim call ------------------------------------------------------------------
-- | Convert a primitive call to LLVM.
convPrimCallM 
        :: Show a 
        => Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper              -- ^ Metadata for the enclosing super
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.Prim               -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> LlvmM (Seq AnnotInstr)

convPrimCallM pp kenv tenv mdsup mdst p _tPrim xs
 = case p of
        -- Binary operations ----------
        A.PrimOp op
         | C.XType t : args     <- xs
         , Just [x1', x2']      <- mconvAtoms pp kenv tenv args
         , Just dst             <- mdst
         -> let result
                 | Just op'     <- convPrimOp2 op t
                 = IOp dst op' x1' x2'

                 | Just icond'  <- convPrimICond op t
                 = IICmp dst icond' x1' x2'

                 | Just fcond'  <- convPrimFCond op t
                 = IFCmp dst fcond' x1' x2'

                 | otherwise
                 = die $ "Invalid binary primop."
           in   return $ Seq.singleton (annotNil result)

        -- Cast primops ---------------
        A.PrimCast A.PrimCastPromote
         | [C.XType tDst, C.XType tSrc, xSrc] <- xs
         , Just xSrc'           <- mconvAtom pp kenv tenv xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimPromote pp kenv tDst vDst tSrc xSrc'
         -> case minstr of
                Just instr      -> return $ Seq.singleton (annotNil instr)
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid promotion of numeric value."
                                , text "  from type: " <> ppr tSrc
                                , text "    to type: " <> ppr tDst]

        A.PrimCast A.PrimCastTruncate
         | [C.XType tDst, C.XType tSrc, xSrc] <- xs
         , Just xSrc'           <- mconvAtom pp kenv tenv xSrc
         , Just vDst            <- mdst
         , minstr               <- convPrimTruncate pp kenv tDst vDst tSrc xSrc'
         -> case minstr of
                Just instr      -> return $ Seq.singleton (annotNil instr)
                Nothing         -> dieDoc $ vcat
                                [ text "Invalid truncation of numeric value."
                                , text " from type: " <> ppr tSrc
                                , text "   to type: " <> ppr tDst ]

        -- Store primops --------------
        A.PrimStore A.PrimStoreSize 
         | [C.XType t]          <- xs
         , Just vDst            <- mdst
         -> let t'      = convType pp kenv t
                size    = case t' of
                            TPointer _           -> platformAddrBytes pp   
                            TInt bits
                             | bits `mod` 8 == 0 -> bits `div` 8
                            _                    -> sorry

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                sorry           = dieDoc $ vcat
                                [ text "  Invalid type applied to size#."]

            in return   $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size))


        A.PrimStore A.PrimStoreSize2
         | [C.XType t]          <- xs
         , Just vDst            <- mdst
         -> let t'      = convType pp kenv t
                size    = case t' of
                            TPointer _           -> platformAddrBytes pp   
                            TInt bits   
                             | bits `mod` 8 == 0 -> bits `div` 8
                            _                    -> sorry

                size2   = truncate $ (log (fromIntegral size) / log 2 :: Double)

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                sorry           = dieDoc $ vcat
                                [ text "  Invalid type applied to size2#."]

            in return   $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size2))


        A.PrimStore A.PrimStoreCreate
         | Just [xBytes']         <- mconvAtoms pp kenv tenv xs
         -> do  vAddr   <- newUniqueNamedVar "addr" (tAddr pp)
                vMax    <- newUniqueNamedVar "max"  (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC_Runtime_heapTop") (TPointer (tAddr pp))
                let vMaxPtr = Var (NameGlobal "_DDC_Runtime_heapMax") (TPointer (tAddr pp))
                return  $ Seq.fromList
                        $ map annotNil
                        [ ICall (Just vAddr) CallTypeStd Nothing
                                (tAddr pp) (NameGlobal "malloc") 
                                [xBytes'] []                         

                        -- Store the top-of-heap pointer
                        , IStore (XVar vTopPtr) (XVar vAddr)

                        -- Store the maximum heap pointer 
                        , IOp    vMax OpAdd     (XVar vAddr) xBytes'
                        , IStore (XVar vMaxPtr) (XVar vMax) ]


        A.PrimStore A.PrimStoreCheck
         | Just [xBytes']         <- mconvAtoms pp kenv tenv xs
         , Just vDst@(Var nDst _) <- mdst
         -> do  let vTop    = Var (bumpName nDst "top") (tAddr pp)
                let vMin    = Var (bumpName nDst "min") (tAddr pp)
                let vMax    = Var (bumpName nDst "max") (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC_Runtime_heapTop") (TPointer (tAddr pp))
                let vMaxPtr = Var (NameGlobal "_DDC_Runtime_heapMax") (TPointer (tAddr pp))
                return  $ Seq.fromList
                        $ map annotNil
                        [ ILoad vTop (XVar vTopPtr)
                        , IOp   vMin OpAdd (XVar vTop) xBytes'
                        , ILoad vMax (XVar vMaxPtr)
                        , IICmp vDst ICondUlt (XVar vMin) (XVar vMax) ]

        A.PrimStore A.PrimStoreAlloc
         | Just vDst@(Var nDst _) <- mdst
         , Just [xBytes']         <- mconvAtoms pp kenv tenv xs
         -> do  let vBump   = Var (bumpName nDst "bump") (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC_Runtime_heapTop") (TPointer (tAddr pp))
                return  $ Seq.fromList
                        $ map annotNil
                        [ ILoad  vDst  (XVar vTopPtr) 
                        , IOp    vBump OpAdd (XVar vDst) xBytes'
                        , IStore (XVar vTopPtr) (XVar vBump)]

        A.PrimStore A.PrimStoreRead
         | C.XType _t : args             <- xs
         , Just [xAddr', xOffset']      <- mconvAtoms pp kenv tenv args
         , Just vDst@(Var nDst tDst)    <- mdst
         -> let vOff    = Var (bumpName nDst "off") (tAddr pp)
                vPtr    = Var (bumpName nDst "ptr") (tPtr tDst)
            in  return  $ Seq.fromList
                        $ map annotNil
                        [ IOp   vOff OpAdd xAddr' xOffset'
                        , IConv vPtr ConvInttoptr (XVar vOff)
                        , ILoad vDst (XVar vPtr) ]

        A.PrimStore A.PrimStoreWrite
         | C.XType _t : args              <- xs
         , Just [xAddr', xOffset', xVal'] <- mconvAtoms pp kenv tenv args      
         -> do  vOff    <- newUniqueNamedVar "off" (tAddr pp)
                vPtr    <- newUniqueNamedVar "ptr" (tPtr $ typeOfExp xVal')
                return  $ Seq.fromList
                        $ map annotNil
                        [ IOp    vOff OpAdd xAddr' xOffset'
                        , IConv  vPtr ConvInttoptr (XVar vOff)
                        , IStore (XVar vPtr) xVal' ]

        A.PrimStore A.PrimStorePlusAddr
         | Just [xAddr', xOffset']      <- mconvAtoms pp kenv tenv xs
         , Just vDst                    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpAdd xAddr' xOffset'

        A.PrimStore A.PrimStoreMinusAddr
         | Just [xAddr', xOffset']      <- mconvAtoms pp kenv tenv xs
         , Just vDst                    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpSub xAddr' xOffset'

        A.PrimStore A.PrimStorePeek
         | C.XType _r : C.XType tDst : args     <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp kenv tenv args
         , Just vDst@(Var nDst _)       <- mdst
         , tDst'                        <- convType   pp kenv tDst
         -> let vAddr1   = Var (bumpName nDst "addr1") (tAddr pp)
                vAddr2   = Var (bumpName nDst "addr2") (tAddr pp)
                vPtr     = Var (bumpName nDst "ptr")   (tPtr tDst')
            in  return  $ Seq.fromList
                        $ (map annotNil
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2) ])                        
                        ++ [(annot kenv mdsup xs
                        ( ILoad vDst  (XVar vPtr)))]

        A.PrimStore A.PrimStorePoke
         | C.XType _r : C.XType tDst : args     <- xs
         , Just [xPtr', xOffset', xVal'] <- mconvAtoms pp kenv tenv args
         , tDst'                         <- convType   pp kenv tDst
         -> do  vAddr1  <- newUniqueNamedVar "addr1" (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2" (tAddr pp)
                vPtr    <- newUniqueNamedVar "ptr"   (tPtr tDst')
                return  $ Seq.fromList
                        $ (map annotNil
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2) ])
                        ++ [(annot kenv mdsup xs 
                        ( IStore (XVar vPtr) xVal' ))]

        A.PrimStore A.PrimStorePlusPtr
         | _xRgn : _xType : args        <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp kenv tenv args
         , Just vDst                    <- mdst
         -> do  vAddr   <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList
                        $ map annotNil
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]

        A.PrimStore A.PrimStoreMinusPtr
         | _xRgn : _xType : args        <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp kenv tenv args
         , Just vDst                    <- mdst
         -> do  vAddr   <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2  <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList
                        $ map annotNil
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpSub (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]

        A.PrimStore A.PrimStoreMakePtr
         | [C.XType _r, C.XType _t, xAddr] <- xs
         , Just xAddr'  <- mconvAtom pp kenv tenv xAddr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvInttoptr xAddr'

        A.PrimStore A.PrimStoreTakePtr
         | [C.XType _r, C.XType _t, xPtr]          <- xs
         , Just xPtr'   <- mconvAtom pp kenv tenv xPtr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint xPtr'

        A.PrimStore A.PrimStoreCastPtr
         | [C.XType _r, C.XType _tSrc, C.XType _tDst, xPtr] <- xs
         , Just xPtr'   <- mconvAtom pp kenv tenv xPtr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvBitcast xPtr'

        _ -> die $ unlines
                [ "Invalid prim call."
                , show (p, xs) ]


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
        -> KindEnv A.Name
        -> C.Type A.Name -> Var 
        -> C.Type A.Name -> Exp
        -> Maybe Instr

convPrimPromote pp kenv tDst vDst tSrc xSrc
 = let  tDst'   = convType pp kenv tDst
        tSrc'   = convType pp kenv tSrc
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
        -> KindEnv A.Name
        -> C.Type  A.Name -> Var
        -> C.Type  A.Name -> Exp
        -> Maybe Instr

convPrimTruncate pp kenv tDst vDst tSrc xSrc
 = let  tDst'   = convType pp kenv tDst
        tSrc'   = convType pp kenv tSrc
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

