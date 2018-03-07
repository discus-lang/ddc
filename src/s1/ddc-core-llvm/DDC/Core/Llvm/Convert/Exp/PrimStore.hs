
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
import qualified DDC.Core.Exp           as C
import qualified Data.Sequence          as Seq
import qualified Data.Text              as Text


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
                let vTopPtr = varGlobal pp "ddcHeapTop"
                let vMaxPtr = varGlobal pp "ddcHeapMax"
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
                let vTopPtr = varGlobal pp "ddcHeapTop"
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


        -- Allocate a new gcroot and set it to the given value.
        A.PrimStore A.PrimStoreAllocSlotVal
         | Just vDst@(Var nDst _)       <- mdst
         , A.RType{} : args             <- as
         , Just [mVal]                  <- atomsR args
         -> Just $ do
                xVal'           <- mVal
                let vRoot       = Var (bumpName nDst "i8") (tPtr (tPtr (TInt 8)))
                return  $ Seq.fromList $ map annotNil
                        [ IAlloca vDst (tPtr (tObj pp))
                        , IStore  (XVar vDst) xVal'
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


        -- Copy a block of memory.
        A.PrimStore A.PrimStoreSet
         | Just [mDst, mVal, mLen]      <- atomsR as
         -> Just $ do
                xDst'    <- mDst
                xVal'    <- mVal
                xLen'    <- mLen
                vDstPtr  <- newUniqueNamedVar "dst" (TPointer (TInt 8))

                -- Alignment of zero means unaligned
                let xAlign      = XLit (LitInt (TInt 32) 0)
                let xVolatile   = XLit (LitInt (TInt 1)  0)
                return  $ Seq.fromList $ map annotNil
                        [ IConv  vDstPtr ConvInttoptr xDst'
                        , ICall Nothing CallTypeStd Nothing
                                TVoid (nameGlobalMemset pp)
                                [ XVar vDstPtr
                                , xVal'
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
         | A.RType{} : A.RType _tDst : args <- as
         , Just vDst                        <- mdst
         , Just [mPtr]                      <- atomsR args
         -> Just $ do
                xPtr'        <- mPtr
                return  $ Seq.fromList
                        $ [(annot kenv mdsup as
                        ( ILoad vDst  xPtr'))]


        -- Write to a raw address.
        A.PrimStore A.PrimStorePoke
         | A.RType{} : A.RType _tDst : args     <- as
         , Just [mPtr, mVal]                    <- atomsR args
         -> Just $ do
                xPtr'    <- mPtr
                xVal'    <- mVal
                return  $ Seq.fromList
                        $  [(annot kenv mdsup as
                        ( IStore xPtr' xVal' ))]


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


        -- Refer to a global variable.
        A.PrimStore A.PrimStoreGlobal
         | [A.RType t, A.RExp x]        <- as
         ,  A.XCon (C.DaConPrim n _)    <- x
         ,  A.NamePrimLit (A.PrimLitTextLit txName) <- n
         ,  Just vDst   <- mdst
         -> Just $ do
                t'      <- convertType pp kenv t
                let vPtr = Var (NameGlobal (Text.unpack txName)) (TPointer t')
                return  $ Seq.singleton $ annotNil
                        $ IConv vDst ConvPtrtoint (XVar vPtr)

        _ -> Nothing


-- | Append the given string to a name.
bumpName :: Name -> String -> Name
bumpName nn s
 = case nn of
        NameLocal str   -> NameLocal  (str ++ "." ++ s)
        NameGlobal str  -> NameGlobal (str ++ "." ++ s)

