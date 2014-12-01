
module DDC.Core.Llvm.Convert.Exp.PrimStore
        (convPrimStore)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Salt.Platform
import Data.Sequence            (Seq)
import qualified DDC.Core.Exp   as C
import qualified DDC.Core.Salt  as A
import qualified Data.Sequence  as Seq


-- | Convert a primitive store operation to LLVM, 
--   or Nothing if this does not look like such an operation.
convPrimStore
        :: Show a
        => Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimStore ctx mdst p _tPrim xs
 = let  pp      = contextPlatform ctx
        mdsup   = contextMDSuper  ctx
        kenv    = contextKindEnv  ctx
        atom    = mconvAtom       ctx
        atoms a = sequence $ map (mconvAtom ctx) a

   in case p of

        -- Get the size in bytes of some primitive type.
        A.PrimStore A.PrimStoreSize
         | [C.XType _ t]        <- xs
         , Just vDst            <- mdst
         -> Just $ do
                t'      <- convertType pp kenv t

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                size    
                 <- case t' of
                        TPointer _           -> return $ platformAddrBytes pp
                        TInt bits
                         | bits `rem` 8 == 0 -> return $ bits `div` 8
                        _ -> throw "invalid type applied to size#"

                return  $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size))


        -- Get the log2 size in bytes of some primtive type.
        A.PrimStore A.PrimStoreSize2
         | [C.XType _ t]        <- xs
         , Just vDst            <- mdst
         -> Just $ do
                t'      <- convertType pp kenv t

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                size    
                 <- case t' of
                        TPointer _              -> return $ platformAddrBytes pp
                        TInt bits
                          |  bits `rem` 8 == 0  -> return $ bits `div` 8

                        _ -> throw "invalid type applied to size2#"

                let size2   
                        = truncate $ (log (fromIntegral size) / log 2 :: Double)

                return  $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size2))


        -- Create the initial heap.
        -- This is called once when the program starts.
        A.PrimStore A.PrimStoreCreate
         | Just [mBytes]                <- atoms xs
         -> Just $ do
                xBytes' <- mBytes
                vAddr   <- newUniqueNamedVar "addr" (tAddr pp)
                vMax    <- newUniqueNamedVar "max"  (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC__heapTop") (TPointer (tAddr pp))
                let vMaxPtr = Var (NameGlobal "_DDC__heapMax") (TPointer (tAddr pp))
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


        -- Check that there is enough space to allocate a new heap object
        -- of the given number of bytes in length.
        A.PrimStore A.PrimStoreCheck
         | Just vDst@(Var nDst _)       <- mdst
         , Just [mBytes]                <- atoms xs
         -> Just $ do
                xBytes'     <- mBytes
                let vTop    = Var (bumpName nDst "top") (tAddr pp)
                let vMin    = Var (bumpName nDst "min") (tAddr pp)
                let vMax    = Var (bumpName nDst "max") (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC__heapTop") (TPointer (tAddr pp))
                let vMaxPtr = Var (NameGlobal "_DDC__heapMax") (TPointer (tAddr pp))
                return  $ Seq.fromList $ map annotNil
                        [ ILoad vTop (XVar vTopPtr)
                        , IOp   vMin OpAdd (XVar vTop) xBytes'
                        , ILoad vMax (XVar vMaxPtr)
                        , IICmp vDst ICondUlt (XVar vMin) (XVar vMax) ]


        -- Allocate a new heap object with the given number of bytes in length.
        A.PrimStore A.PrimStoreAlloc
         | Just vDst@(Var nDst _)       <- mdst
         , Just [mBytes]                <- atoms xs
         -> Just $ do
                xBytes'     <- mBytes
                let vBump   = Var (bumpName nDst "bump") (tAddr pp)
                let vTopPtr = Var (NameGlobal "_DDC__heapTop") (TPointer (tAddr pp))
                return  $ Seq.fromList $ map annotNil
                        [ ILoad  vDst  (XVar vTopPtr)
                        , IOp    vBump OpAdd (XVar vDst) xBytes'
                        , IStore (XVar vTopPtr) (XVar vBump)]


        -- Read a value via a pointer.
        A.PrimStore A.PrimStoreRead
         | C.XType{} : args             <- xs
         , Just vDst@(Var nDst tDst)    <- mdst
         , Just [mAddr, mOffset]        <- atoms args
         -> Just $ do
                xAddr'      <- mAddr
                xOffset'    <- mOffset
                let vOff    = Var (bumpName nDst "off") (tAddr pp)
                let vPtr    = Var (bumpName nDst "ptr") (tPtr tDst)
                return  $ Seq.fromList $ map annotNil
                        [ IOp   vOff OpAdd xAddr' xOffset'
                        , IConv vPtr ConvInttoptr (XVar vOff)
                        , ILoad vDst (XVar vPtr) ]


        -- Write a value via a pointer.
        A.PrimStore A.PrimStoreWrite
         | C.XType{} : args             <- xs
         , Just [mAddr, mOffset, mVal]  <- atoms args
         -> Just $ do
                xAddr'   <- mAddr
                xOffset' <- mOffset
                xVal'    <- mVal
                vOff     <- newUniqueNamedVar "off" (tAddr pp)
                vPtr     <- newUniqueNamedVar "ptr" (tPtr $ typeOfExp xVal')
                return  $ Seq.fromList $ map annotNil
                        [ IOp    vOff OpAdd xAddr' xOffset'
                        , IConv  vPtr ConvInttoptr (XVar vOff)
                        , IStore (XVar vPtr) xVal' ]


        -- Add an offset in bytes to a pointer.
        A.PrimStore A.PrimStorePlusAddr
         | Just vDst                    <- mdst
         , Just [mAddr, mOffset]        <- atoms xs
         -> Just $ do
                xAddr'   <- mAddr
                xOffset' <- mOffset
                return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpAdd xAddr' xOffset'


        -- Subtract an offset in bytes from a pointer.
        A.PrimStore A.PrimStoreMinusAddr
         | Just vDst                    <- mdst
         , Just [mAddr, mOffset]        <- atoms xs
         -> Just $ do
                xAddr'       <- mAddr
                xOffset'     <- mOffset
                return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpSub xAddr' xOffset'


        -- Read from a raw address.
        A.PrimStore A.PrimStorePeek
         | C.XType{} : C.XType _ tDst : args     <- xs
         , Just vDst@(Var nDst _)       <- mdst
         , Just [mPtr, mOffset]         <- atoms args
         -> Just $ do
                tDst'        <- convertType   pp kenv tDst
                xPtr'        <- mPtr
                xOffset'     <- mOffset
                let vAddr1   = Var (bumpName nDst "addr1") (tAddr pp)
                let vAddr2   = Var (bumpName nDst "addr2") (tAddr pp)
                let vPtr     = Var (bumpName nDst "ptr")   (tPtr tDst')
                return  $ Seq.fromList
                        $ (map annotNil
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2) ])
                        ++ [(annot kenv mdsup xs
                        ( ILoad vDst  (XVar vPtr)))]


        -- Write to a raw address.
        A.PrimStore A.PrimStorePoke
         | C.XType{} : C.XType _ tDst : args    <- xs
         , Just [mPtr, mOffset, mVal]           <- atoms args
         -> Just $ do
                tDst'    <- convertType pp kenv tDst
                xPtr'    <- mPtr
                xOffset' <- mOffset
                xVal'    <- mVal
                vAddr1   <- newUniqueNamedVar "addr1" (tAddr pp)
                vAddr2   <- newUniqueNamedVar "addr2" (tAddr pp)
                vPtr     <- newUniqueNamedVar "ptr"   (tPtr tDst')
                return  $ Seq.fromList
                        $ (map annotNil
                        [ IConv vAddr1 ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr1) xOffset'
                        , IConv vPtr   ConvInttoptr (XVar vAddr2) ])
                        ++ [(annot kenv mdsup xs
                        ( IStore (XVar vPtr) xVal' ))]


        -- Add an offset to a raw address.
        A.PrimStore A.PrimStorePlusPtr
         | _xRgn : _xType : args        <- xs
         , Just vDst                    <- mdst
         , Just [mPtr, mOffset]         <- atoms args
         -> Just $ do
                xPtr'    <- mPtr
                xOffset' <- mOffset
                vAddr    <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2   <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList $ map annotNil
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpAdd (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]


        -- Subtrace an offset from a raw address.
        A.PrimStore A.PrimStoreMinusPtr
         | _xRgn : _xType : args        <- xs
         , Just vDst                    <- mdst
         , Just [mPtr, mOffset]         <- atoms args
         -> Just $ do
                xPtr'    <- mPtr
                xOffset' <- mOffset
                vAddr    <- newUniqueNamedVar "addr"   (tAddr pp)
                vAddr2   <- newUniqueNamedVar "addr2"  (tAddr pp)
                return  $ Seq.fromList $ map annotNil
                        [ IConv vAddr  ConvPtrtoint xPtr'
                        , IOp   vAddr2 OpSub (XVar vAddr) xOffset'
                        , IConv vDst   ConvInttoptr (XVar vAddr2) ]


        -- Construct a pointer from an address.
        A.PrimStore A.PrimStoreMakePtr
         | [C.XType{}, C.XType{}, xAddr] <- xs
         , Just vDst    <- mdst
         , Just mAddr   <- atom xAddr
         -> Just $ do
                xAddr'  <- mAddr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvInttoptr xAddr'


        -- Take an address from a pointer.
        A.PrimStore A.PrimStoreTakePtr
         | [C.XType{}, C.XType{}, xPtr] <- xs
         , Just vDst    <- mdst
         , Just mPtr    <- atom xPtr
         -> Just $ do
                xPtr'   <- mPtr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint xPtr'


        -- Case a pointer from one type to another.
        A.PrimStore A.PrimStoreCastPtr
         | [C.XType{}, C.XType{}, C.XType{}, xPtr] <- xs
         , Just vDst    <- mdst
         , Just mPtr    <- atom xPtr
         -> Just $ do  
                xPtr'   <- mPtr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvBitcast xPtr'

        _ -> Nothing


-- | Append the given string to a name.
bumpName :: Name -> String -> Name
bumpName nn s
 = case nn of
        NameLocal str   -> NameLocal  (str ++ "." ++ s)
        NameGlobal str  -> NameGlobal (str ++ "." ++ s)

