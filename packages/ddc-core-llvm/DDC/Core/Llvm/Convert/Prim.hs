
module DDC.Core.Llvm.Convert.Prim
        (convPrimCallM)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
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
        -> A.PrimOp             -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> LlvmM (Seq AnnotInstr)

convPrimCallM pp kenv tenv mdsup mdst p _tPrim xs
 = case p of
        -- Binary operations ----------
        A.PrimArith op
         | C.XType _ t : args     <- xs
         , Just [x1', x2']      <- mconvAtoms pp kenv tenv args
         , Just dst             <- mdst
         -> let result
                 | Just op'     <- convPrimArith2 op t
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
         | [C.XType _ tDst, C.XType _ tSrc, xSrc] <- xs
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
         | [C.XType _ tDst, C.XType _ tSrc, xSrc] <- xs
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
         | [C.XType _ t]        <- xs
         , Just vDst            <- mdst
         -> let t'      = convertType pp kenv t
                size    = case t' of
                            TPointer _           -> platformAddrBytes pp   
                            TInt bits
                             | bits `rem` 8 == 0 -> bits `div` 8
                            _                    -> sorry

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                sorry           = dieDoc $ vcat
                                [ text "  Invalid type applied to size#."]

            in return   $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size))


        A.PrimStore A.PrimStoreSize2
         | [C.XType _ t]        <- xs
         , Just vDst            <- mdst
         -> let t'      = convertType pp kenv t
                size    = case t' of
                            TPointer _           -> platformAddrBytes pp
                            TInt bits
                             | bits `rem` 8 == 0 -> bits `div` 8
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
         | C.XType{} : args             <- xs
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
         | C.XType{} : args              <- xs
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
         | C.XType{} : C.XType _ tDst : args     <- xs
         , Just [xPtr', xOffset']       <- mconvAtoms pp kenv tenv args
         , Just vDst@(Var nDst _)       <- mdst
         , tDst'                        <- convertType   pp kenv tDst
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
         | C.XType{} : C.XType _ tDst : args     <- xs
         , Just [xPtr', xOffset', xVal'] <- mconvAtoms pp kenv tenv args
         , tDst'                         <- convertType   pp kenv tDst
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
         | [C.XType{}, C.XType{}, xAddr] <- xs
         , Just xAddr'  <- mconvAtom pp kenv tenv xAddr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvInttoptr xAddr'

        A.PrimStore A.PrimStoreTakePtr
         | [C.XType{}, C.XType{}, xPtr]          <- xs
         , Just xPtr'   <- mconvAtom pp kenv tenv xPtr
         , Just vDst    <- mdst
         ->     return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint xPtr'

        A.PrimStore A.PrimStoreCastPtr
         | [C.XType{}, C.XType{}, C.XType{}, xPtr] <- xs
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
convPrimArith2 :: A.PrimArith -> C.Type A.Name -> Maybe Op
convPrimArith2 op t
 = case op of
        A.PrimArithAdd     
         | isIntegralT t                -> Just OpAdd
         | isFloatingT t                -> Just OpFAdd 

        A.PrimArithSub      
         | isIntegralT t                -> Just OpSub
         | isFloatingT t                -> Just OpFSub

        A.PrimArithMul 
         | isIntegralT t                -> Just OpMul
         | isFloatingT t                -> Just OpFMul

        A.PrimArithDiv
         | isIntegralT t, isUnsignedT t -> Just OpUDiv
         | isIntegralT t, isSignedT t   -> Just OpSDiv
         | isFloatingT t                -> Just OpFDiv

        A.PrimArithRem
         | isIntegralT t, isUnsignedT t -> Just OpURem
         | isIntegralT t, isSignedT t   -> Just OpSRem
         | isFloatingT t                -> Just OpFRem

        A.PrimArithShl
         | isIntegralT t                -> Just OpShl

        A.PrimArithShr
         | isIntegralT t, isUnsignedT t -> Just OpLShr
         | isIntegralT t, isSignedT t   -> Just OpAShr

        A.PrimArithBAnd
         | isIntegralT t                -> Just OpAnd

        A.PrimArithBOr
         | isIntegralT t                -> Just OpOr

        A.PrimArithBXOr
         | isIntegralT t                -> Just OpXor

        _                               -> Nothing


-- Cast -----------------------------------------------------------------------
-- | Convert a primitive promotion to LLVM, 
--   or `Nothing` for an invalid promotion.
convPrimPromote 
        :: Platform 
        -> KindEnv A.Name
        -> C.Type A.Name -> Var 
        -> C.Type A.Name -> Exp
        -> Maybe Instr

convPrimPromote pp kenv tDst vDst tSrc xSrc
 | tSrc'        <- convertType pp kenv tSrc
 , tDst'        <- convertType pp kenv tDst
 , Just (A.NamePrimTyCon tcSrc, _) <- takePrimTyConApps tSrc
 , Just (A.NamePrimTyCon tcDst, _) <- takePrimTyConApps tDst 
 , A.primCastPromoteIsValid pp tcSrc tcDst
 = case (tDst', tSrc') of
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

 | otherwise
 = Nothing


-- | Convert a primitive truncation to LLVM, 
--   or `Nothing` for an invalid truncation.
convPrimTruncate
        :: Platform 
        -> KindEnv A.Name
        -> C.Type  A.Name -> Var
        -> C.Type  A.Name -> Exp
        -> Maybe Instr

convPrimTruncate pp kenv tDst vDst tSrc xSrc
 | tSrc'        <- convertType pp kenv tSrc
 , tDst'        <- convertType pp kenv tDst
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
         , isUnsignedT tSrc
         , isSignedT   tDst
         -> Just $ IConv vDst ConvZext xSrc

        _ -> Nothing

 | otherwise
 = Nothing


-- Cond -----------------------------------------------------------------------
-- | Convert an integer comparison from Core Sea to LLVM form.
convPrimICond :: A.PrimArith -> C.Type A.Name -> Maybe ICond
convPrimICond op t
 | isIntegralT t
 = case op of
        A.PrimArithEq   -> Just ICondEq
        A.PrimArithNeq  -> Just ICondNe
        A.PrimArithGt   -> Just ICondUgt
        A.PrimArithGe   -> Just ICondUge
        A.PrimArithLt   -> Just ICondUlt
        A.PrimArithLe   -> Just ICondUle
        _               -> Nothing

 | otherwise            =  Nothing


-- | Convert a floating point comparison from Core Sea to LLVM form.
convPrimFCond :: A.PrimArith -> C.Type A.Name -> Maybe FCond
convPrimFCond op t
 | isIntegralT t
 = case op of
        A.PrimArithEq   -> Just FCondOeq
        A.PrimArithNeq  -> Just FCondOne
        A.PrimArithGt   -> Just FCondOgt
        A.PrimArithGe   -> Just FCondOge
        A.PrimArithLt   -> Just FCondOlt
        A.PrimArithLe   -> Just FCondOle
        _               -> Nothing

 | otherwise            =  Nothing

