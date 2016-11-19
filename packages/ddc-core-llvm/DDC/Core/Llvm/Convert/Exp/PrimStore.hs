
module DDC.Core.Llvm.Convert.Exp.PrimStore
        (convPrimStore)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.Runtime
import DDC.Core.Salt.Platform
import DDC.Core.Exp.Generic.BindStruct  ()
import Data.Sequence                    (Seq)
import qualified DDC.Core.Salt          as A
import qualified Data.Sequence          as Seq


-- | Convert a primitive store operation to LLVM, 
--   or Nothing if this does not look like such an operation.
convPrimStore
        :: Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Prim to call.
        -> [A.Arg]              -- ^ Arguments to prim.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimStore ctx mdst p as
 = let  pp         = contextPlatform ctx
        mdsup      = contextMDSuper  ctx
        kenv       = contextKindEnv  ctx
        atom       = mconvAtom       ctx
        atomsR as' = sequence $ map (mconvArg ctx) as'

   in case p of

        -- Get the size in bytes of some primitive type.
        A.PrimStore A.PrimStoreSize
         | [A.RType t]  <- as
         , Just vDst    <- mdst
         -> Just $ do
                t'      <- convertType pp kenv t

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                size    
                 <- case t' of
                        TPointer _           -> return $ platformAddrBytes pp
                        TInt bits
                         | bits `rem` 8 == 0 -> return $ bits `div` 8
                        _ -> throw $ ErrorInvalidSizeType t

                return  $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size))


        -- Get the log2 size in bytes of some primtive type.
        A.PrimStore A.PrimStoreSize2
         | [A.RType t]  <- as
         , Just vDst    <- mdst
         -> Just $ do
                t'      <- convertType pp kenv t

                -- Bool# is only 1 bit long.
                -- Don't return a result for types that don't divide into 8 bits evenly.
                size    
                 <- case t' of
                        TPointer _              -> return $ platformAddrBytes pp
                        TInt bits
                          |  bits `rem` 8 == 0  -> return $ bits `div` 8

                        _ -> throw $ ErrorInvalidSize2Type t

                let size2   
                        = truncate $ (log (fromIntegral size) / log 2 :: Double)

                return  $ Seq.singleton
                        $ annotNil
                        $ ISet vDst (XLit (LitInt (tNat pp) size2))


        -- Create the initial heap.
        -- This is called once when the program starts.
        A.PrimStore A.PrimStoreCreate
         | Just [mBytes]    <- atomsR as
         -> Just $ do
                xBytes'     <- mBytes
                vAddr       <- newUniqueNamedVar "addr" (tAddr pp)
                vMax        <- newUniqueNamedVar "max"  (tAddr pp)
                let vTopPtr =  varGlobalHeapTop pp
                let vMaxPtr =  varGlobalHeapMax pp
                return  $ Seq.fromList
                        $ map annotNil
                        [ ICall (Just vAddr) CallTypeStd Nothing
                                (tAddr pp) nameGlobalMalloc
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
         , Just [mBytes]                <- atomsR as
         -> Just $ do
                xBytes'     <- mBytes
                let vTop    = Var (bumpName nDst "top") (tAddr pp)
                let vMin    = Var (bumpName nDst "min") (tAddr pp)
                let vMax    = Var (bumpName nDst "max") (tAddr pp)
                let vTopPtr = varGlobalHeapTop pp
                let vMaxPtr = varGlobalHeapMax pp
                return  $ Seq.fromList $ map annotNil
                        [ ILoad vTop (XVar vTopPtr)
                        , IOp   vMin OpAdd (XVar vTop) xBytes'
                        , ILoad vMax (XVar vMaxPtr)
                        , ICmp  vDst (ICond ICondUlt) (XVar vMin) (XVar vMax) ]


        -- Allocate a new heap object with the given number of bytes in length.
        A.PrimStore A.PrimStoreAlloc
         | Just vDst@(Var nDst _)       <- mdst
         , Just [mBytes]                <- atomsR as
         -> Just $ do
                xBytes'     <- mBytes
                let vBump   = Var (bumpName nDst "bump") (tAddr pp)
                let vTopPtr = varGlobalHeapTop pp
                return  $ Seq.fromList $ map annotNil
                        [ ILoad  vDst  (XVar vTopPtr)
                        , IOp    vBump OpAdd (XVar vDst) xBytes'
                        , IStore (XVar vTopPtr) (XVar vBump)]


        -- Allocate a new gcroot
        A.PrimStore A.PrimStoreAllocSlot
         | Just vDst@(Var nDst _)       <- mdst
         , [A.RType _]                  <- as
         -> Just $ do
                let vRoot       = Var (bumpName nDst "i8") (tPtr (tPtr (TInt 8)))
                return  $ Seq.fromList $ map annotNil
                        [ IAlloca vDst (tPtr (tObj pp))
                        , IStore  (XVar vDst) (XLit (LitNull (tPtr (tObj pp))))
                        , IConv   vRoot ConvBitcast (XVar vDst)
                        , ICall Nothing CallTypeStd Nothing
                                TVoid nameGlobalGcroot
                                [ XVar vRoot
                                , XLit (LitNull (tPtr (TInt 8)))
                                ] [] ]


        -- Read a value via a pointer.
        A.PrimStore A.PrimStoreRead
         | A.RType{} : args             <- as
         , Just vDst@(Var nDst tDst)    <- mdst
         , Just [mAddr, mOffset]        <- atomsR args
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
         | A.RType{} : args             <- as
         , Just [mAddr, mOffset, mVal]  <- atomsR args
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


        -- Copy a block of memory.
        A.PrimStore A.PrimStoreCopy
         | Just [mDst, mSrc, mLen]      <- atomsR as
         -> Just $ do
                xDst'    <- mDst
                xSrc'    <- mSrc
                xLen'    <- mLen
                vDstPtr  <- newUniqueNamedVar "dst" (TPointer (TInt 8))
                vSrcPtr  <- newUniqueNamedVar "src" (TPointer (TInt 8))

                -- Alignment of zero means unaligned
                let xAlign      = XLit (LitInt (TInt 32) 0)
                let xVolatile   = XLit (LitInt (TInt 1)  0)
                return  $ Seq.fromList $ map annotNil
                        [ IConv  vDstPtr ConvInttoptr xDst'
                        , IConv  vSrcPtr ConvInttoptr xSrc'
                        , ICall Nothing CallTypeStd Nothing
                                TVoid (nameGlobalMemcpy pp)
                                [ XVar vDstPtr
                                , XVar vSrcPtr
                                , xLen'
                                , xAlign
                                , xVolatile
                                ] [] ]


        -- Add an offset in bytes to a pointer.
        A.PrimStore A.PrimStorePlusAddr
         | Just vDst                    <- mdst
         , Just [mAddr, mOffset]        <- atomsR as
         -> Just $ do
                xAddr'   <- mAddr
                xOffset' <- mOffset
                return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpAdd xAddr' xOffset'


        -- Subtract an offset in bytes from a pointer.
        A.PrimStore A.PrimStoreMinusAddr
         | Just vDst                    <- mdst
         , Just [mAddr, mOffset]        <- atomsR as
         -> Just $ do
                xAddr'       <- mAddr
                xOffset'     <- mOffset
                return  $ Seq.singleton $ annotNil
                        $ IOp vDst OpSub xAddr' xOffset'


        -- Read from a raw address.
        A.PrimStore A.PrimStorePeek
         | A.RType{} : A.RType tDst : args      <- as
         , Just vDst@(Var nDst _)               <- mdst
         , Just [mPtr, mOffset]                 <- atomsR args
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
                        ++ [(annot kenv mdsup as
                        ( ILoad vDst  (XVar vPtr)))]


        -- Write to a raw address.
        A.PrimStore A.PrimStorePoke
         | A.RType{} : A.RType tDst : args      <- as
         , Just [mPtr, mOffset, mVal]           <- atomsR args
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
                        ++ [(annot kenv mdsup as
                        ( IStore (XVar vPtr) xVal' ))]


        -- Add an offset to a raw address.
        A.PrimStore A.PrimStorePlusPtr
         | _xRgn : _xType : args        <- as
         , Just vDst                    <- mdst
         , Just [mPtr, mOffset]         <- atomsR args
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
         | _xRgn : _xType : args        <- as
         , Just vDst                    <- mdst
         , Just [mPtr, mOffset]         <- atomsR args
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
         | [A.RType{}, A.RType{}, A.RExp xAddr] <- as
         , Just vDst    <- mdst
         , Just mAddr   <- atom xAddr
         -> Just $ do
                xAddr'  <- mAddr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvInttoptr xAddr'


        -- Take an address from a pointer.
        A.PrimStore A.PrimStoreTakePtr
         | [A.RType{}, A.RType{}, A.RExp xPtr] <- as
         , Just vDst    <- mdst
         , Just mPtr    <- atom xPtr
         -> Just $ do
                xPtr'   <- mPtr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint xPtr'


        -- Case a pointer from one type to another.
        A.PrimStore A.PrimStoreCastPtr
         | [A.RType{}, A.RType{}, A.RType{}, A.RExp xPtr] <- as
         , Just vDst    <- mdst
         , Just mPtr    <- atom xPtr
         -> Just $ do  
                xPtr'   <- mPtr
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvBitcast xPtr'


        -- The GC root chain.
        A.PrimStore A.PrimStoreRootChain
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                let vRootPtr = varGlobalLlvmRootChain pp
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint (XVar vRootPtr)


        -- The base of the front heap.
        A.PrimStore A.PrimStoreHeapBase
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vBasePtr = varGlobalHeapBase pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vBasePtr)


        -- The top of the front heap.
        A.PrimStore A.PrimStoreHeapTop
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vTopPtr = varGlobalHeapTop pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vTopPtr)


        -- The maximum top of the front heap.
        A.PrimStore A.PrimStoreHeapMax
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vMaxPtr = varGlobalHeapMax pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vMaxPtr)


        -- The base of the back heap.
        A.PrimStore A.PrimStoreHeapBackBase
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vBasePtr = varGlobalHeapBackBase pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vBasePtr)


        -- The top of the back heap.
        A.PrimStore A.PrimStoreHeapBackTop
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vTopPtr = varGlobalHeapBackTop pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vTopPtr)


        -- The maximum top of the back heap.
        A.PrimStore A.PrimStoreHeapBackMax
         | []           <- as
         , Just vDst    <- mdst
         -> Just $ do
                 let vMaxPtr = varGlobalHeapBackMax pp
                 return  $ Seq.singleton $ annotNil
                         $ IConv vDst ConvPtrtoint (XVar vMaxPtr)

        _ -> Nothing


-- | Append the given string to a name.
bumpName :: Name -> String -> Name
bumpName nn s
 = case nn of
        NameLocal str   -> NameLocal  (str ++ "." ++ s)
        NameGlobal str  -> NameGlobal (str ++ "." ++ s)

