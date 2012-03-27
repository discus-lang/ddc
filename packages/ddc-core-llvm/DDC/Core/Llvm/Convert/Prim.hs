
module DDC.Core.Llvm.Convert.Prim
        ( convPrimExtern
        , convPrimOp2
        , convPrimICond
        , convPrimFCond)
where
import DDC.Core.Llvm.Convert.Type
import DDC.Llvm.Prim
import DDC.Llvm.Exp
import qualified DDC.Core.Exp                   as C
import qualified DDC.Core.Sea.Output.Name       as E


-- | Get the symbol name of an external primitive.
convPrimExtern :: E.PrimExternal -> C.Type E.Name -> Maybe Name
convPrimExtern p _t
 = case p of
        E.PrimExternalShowInt bits      -> Just $ NameGlobal ("showInt" ++ show bits ++ "#")
        E.PrimExternalPutStr            -> Just $ NameGlobal "putStr"
        E.PrimExternalPutStrLn          -> Just $ NameGlobal "putStrLn"


-- | Convert a binary primop from Core Sea to LLVM form.
convPrimOp2 :: E.PrimOp -> C.Type E.Name -> Maybe Op
convPrimOp2 op t
 = case op of
        E.PrimOpAdd     
         | isIntegralT t                -> Just OpAdd
         | isFloatingT t                -> Just OpFAdd 

        E.PrimOpSub      
         | isIntegralT t                -> Just OpSub
         | isFloatingT t                -> Just OpFSub

        E.PrimOpMul 
         | isIntegralT t                -> Just OpMul
         | isFloatingT t                -> Just OpFMul

        E.PrimOpDiv
         | isIntegralT t, isUnsignedT t -> Just OpUDiv
         | isIntegralT t, isSignedT t   -> Just OpSDiv
         | isFloatingT t                -> Just OpFDiv

        E.PrimOpRem
         | isIntegralT t, isUnsignedT t -> Just OpURem
         | isIntegralT t, isSignedT t   -> Just OpSRem
         | isFloatingT t                -> Just OpFRem

        E.PrimOpShl
         | isIntegralT t                -> Just OpShl

        E.PrimOpShr
         | isIntegralT t, isUnsignedT t -> Just OpLShr
         | isIntegralT t, isSignedT t   -> Just OpAShr

        E.PrimOpBAnd
         | isIntegralT t                -> Just OpAnd

        E.PrimOpBOr
         | isIntegralT t                -> Just OpOr

        E.PrimOpBXOr
         | isIntegralT t                -> Just OpXor

        _                               -> Nothing


-- | Convert an integer comparison from Core Sea to LLVM form.
convPrimICond :: E.PrimOp -> C.Type E.Name -> Maybe ICond
convPrimICond op t
 | isIntegralT t
 = case op of
        E.PrimOpEq      -> Just ICondEq
        E.PrimOpNeq     -> Just ICondNe
        E.PrimOpGt      -> Just ICondUgt
        E.PrimOpGe      -> Just ICondUge
        E.PrimOpLt      -> Just ICondUlt
        E.PrimOpLe      -> Just ICondUle
        _               -> Nothing

 | otherwise            =  Nothing


-- | Convert a floating point comparison from Core Sea to LLVM form.
convPrimFCond :: E.PrimOp -> C.Type E.Name -> Maybe FCond
convPrimFCond op t
 | isIntegralT t
 = case op of
        E.PrimOpEq      -> Just FCondOeq
        E.PrimOpNeq     -> Just FCondOne
        E.PrimOpGt      -> Just FCondOgt
        E.PrimOpGe      -> Just FCondOge
        E.PrimOpLt      -> Just FCondOlt
        E.PrimOpLe      -> Just FCondOle
        _               -> Nothing

 | otherwise            =  Nothing



