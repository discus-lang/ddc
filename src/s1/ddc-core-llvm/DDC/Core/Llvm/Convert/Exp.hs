
-- In GHC 8 we need to turn of the pattern match checker because the new algorithm
-- runs out of stack space when checking this module.
-- https://ghc.haskell.org/trac/ghc/ticket/11822
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns -w #-}

module DDC.Core.Llvm.Convert.Exp
        ( Context (..)
        , convertSuperInit
        , convertSuperBody
        , convertSimple
        , bindLocalA, bindLocalAs)
where
import DDC.Core.Llvm.Convert.Exp.PrimCall
import DDC.Core.Llvm.Convert.Exp.PrimArith
import DDC.Core.Llvm.Convert.Exp.PrimCast
import DDC.Core.Llvm.Convert.Exp.PrimStore
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Base
import DDC.Llvm.Syntax
import Control.Applicative
import Data.Sequence                            (Seq, (|>), (><))
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Compounds        as A
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Env                   as Env
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map
import qualified Data.Text                      as T

import Debug.Trace

---------------------------------------------------------------------------------------------------
-- | Convert the body of a supercombinator to LLVM blocks.
--
--   The result contains an initial block that contains instructions that allocate
--   and initialize shadow stack slots, then more blocks that implement the rest
--   of the super. We put the slot allocation instructions in a separate block so
--   that we can jump past them when performing a tail call, and this re-use the
--   existing slots for each recursive call.
--
--   The LLVM code for the body of the super then looks like:
--
-- @ l??.init:
--      v???.var = alloca %s.Obj*       -- allocate a stack slot
--      ...                             -- more allocation and init of stack slots
--
--   l??.body:
--      ...                             -- code for the super proper
--
-- @
--
convertSuperInit
        :: Context
        -> A.Exp
        -> ConvertM (Seq Block)

convertSuperInit ctx xBody
 = do   convertSuperAllocs ctx Seq.empty xBody


---------------------------------------------------------------------------------------------------
-- | Convert the prelude of a supercombinator body to LLVM blocks.
--   which consists of bindings that allocate slots on the shadow stack.
convertSuperAllocs
        :: Context
        -> Seq AnnotInstr
        -> A.Exp
        -> ConvertM (Seq Block)

convertSuperAllocs ctx instrs xx
 = case xx of
        -- A binding that allocates a slot on the shadow stack and
        -- associates a Salt level variable name with it.
        -- These slots are used to hold pointers to the super arguments.
        A.XLet (A.LLet  (C.BName nm t) x1) x2
         | Just (A.PrimStore prim, xsArg) <- A.takeXPrimApps x1
         , A.PrimStoreAllocSlotVal        <- prim
         -> do
                -- Bind the Salt name for the slot,
                -- allocating a new LLVM variable name for it.
                (ctx', vSlot)  <- bindLocalV ctx nm t

                -- Convert the bound expression.
                --   As the let-binding is non-recursive, the bound expression
                --   is converted in the original context, without the let-bound
                --   variable (ctx).
                instrs'         <- convertSimple ctx' (ExpAssign ExpTop vSlot) x1


                -- Remember the name of the parameter being written to this new slot.
                let [A.RType _rObj, A.RExp (A.XVar (C.UName nParam))]
                          = xsArg

                let ctx'' = ctx'
                          { contextSuperParamSlots
                          = contextSuperParamSlots ctx' ++ [(nParam, vSlot)] }

                -- Convert the body of the let-expression.
                --   This is done in the new context, with the let-bound variable.
                --   If we need to perform a tail call we can use the mapping between
                --   parameter names and their slots to just update the slots and
                --   jump back to the start of the body of the super.
                convertSuperAllocs ctx'' (instrs >< instrs') x2


        -- A binding that allocates a slot on the shadow stack and
        -- just initializes it to null.
        -- These slots are used to objects created in the body of the super.
        A.XLet (A.LLet  (C.BName nm t) x1) x2
         | Just (A.PrimStore prim, _)   <- A.takeXPrimApps x1
         , A.PrimStoreAllocSlot         <- prim
         -> do
                -- Bind the Salt name, allocating a new LLVM variable name for it.
                (ctx', vDst) <- bindLocalV ctx nm t

                -- Convert the bound expression.
                --   As the let-binding is non-recursive, the bound expression
                --   is converted in the original context, without the let-bound
                --   variable (ctx).
                instrs'      <- convertSimple ctx (ExpAssign ExpTop vDst) x1

                -- Convert the body of the let-expression.
                --   This is done in the new context, with the let-bound variable.
                convertSuperAllocs ctx' (instrs >< instrs') x2


        -- The current binding does not allocate a new slot on the shadow stack,
        -- so assume what we've already seen all the slot allocations in this super.
        --
        --   We create a new block for all the slot allocations,
        --   then branch to the body of the super proper.
        _ -> do
                -- Label for the block that allocates all the shadow stack slots.
                lInit   <- newUniqueLabel "init"

                -- Label for the first block in the super proper.
                lBody   <- newUniqueLabel "body"

                -- Create the block that allcoates all the slots.
                let blkInit
                         = Block lInit
                                (instrs |> (annotNil $ IBranch lBody))

                -- Add the super body label to the context.
                -- If we need to perform a self-tail call we can jump back to this
                -- label instead of needing to perform a proper function call.
                let ctx' = ctx
                         { contextSuperBodyLabel = Just lBody }

                -- Convert the main body of the super.
                convertSuperBody
                        ctx'
                        ExpTop
                        (Seq.singleton blkInit)
                        lBody Seq.empty xx


---------------------------------------------------------------------------------------------------
-- | Convert the body of a supercombinator to LLVM blocks.
--   This is after we've already converted the prelude that allocates shadow stack slots.
convertSuperBody
        :: Context
        -> ExpContext
        -> Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq AnnotInstr       -- ^ Instrs in current block.
        -> A.Exp                -- ^ Expression being converted.
        -> ConvertM (Seq Block) -- ^ Final blocks of function body.

convertSuperBody ctx ectx blocks label instrs xx
 = let  pp           = contextPlatform    ctx
        kenv         = contextKindEnv     ctx
        convertCase  = contextConvertCase ctx
        atomsR as' = sequence $ map (mconvArg ctx) as'
   in do
        case xx of

         -- Control transfer instructions -----------------
         -- Void return applied to a literal void constructor.
         --   We must be at the top-level of the function.
         A.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (p, as)                       <- A.takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [A.RType{}, A.RExp (A.XCon dc)]    <- as
          ,  Just (A.NamePrimLit A.PrimLitVoid) <- A.takeNameOfDaCon dc
          -> return  $   blocks
                     |>  Block label
                               (instrs |> (annotNil $ IReturn Nothing))

         -- Void return applied to some other expression.
         --   We still have to eval the expression, but it returns no value.
         --   We must be at the top-level of the function.
         A.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (p, as)                       <- A.takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [A.RType t, A.RExp x2]             <- as
          ,  isVoidT t
          -> do instrs2 <- convertSimple ctx ectx x2
                return  $  blocks
                        |> Block label
                                 (instrs >< (instrs2 |> (annotNil $ IReturn Nothing)))

         -- Return a value.
         --   We must be at the top-level of the function.
         A.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (p, as)                       <- A.takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [A.RType t, A.RExp x]              <- as
          -> do t'      <- convertType pp kenv t
                vDst    <- newUniqueVar t'
                is      <- convertSimple ctx (ExpAssign ectx vDst) x
                return  $   blocks
                        |>  Block label
                                  (instrs >< (is |> (annotNil $ IReturn (Just (XVar vDst)))))

         -- Fail and abort the program.
         --   Allow this inside an expression as well as from the top level.
         A.XApp{}
          |  Just (p, as)                       <- A.takeXPrimApps xx
          ,  A.PrimControl A.PrimControlFail    <- p
          ,  [A.RType _tResult]                 <- as
          -> let
                 iSet   = case ectx of
                           ExpTop{}           -> INop
                           ExpNest _ vDst _   -> ISet vDst (XUndef (typeOfVar vDst))
                           ExpAssign _ vDst   -> ISet vDst (XUndef (typeOfVar vDst))

                 iFail  = ICall Nothing CallTypeStd Nothing
                                TVoid (NameGlobal "abort") [] []

                 block  = Block label
                        $ instrs |> annotNil iSet
                                 |> annotNil iFail
                                 |> annotNil IUnreachable


             in  return $ blocks |> block


         -- Calls -----------------------------------------
         -- Perform a self tailcall.
         --  This is when the current super being converted tail calls itself.
         --  To perform the call we can just assign the new arguments to their
         --  corresponding shadow stack slots and jump back to the start
         --  of the super body. This avoids pushing new arguments or slots onto
         --  the stack, so the call runs in constant stack space.
         --
         A.XApp{}
          |  Just (prim, args)                           <- A.takeXPrimApps xx
          ,  A.PrimControl (A.PrimControlTailCall arity) <- prim
          ,  arity > 0

          -- Split the arguments to the tailcall# primitive.
          ,  tsArgs                                     <- take arity args
          ,  A.RType _tResult : A.RExp xFunTys : xsArgs <- drop arity args

          -- Get the name of the super being called and check that this is
          -- also the name of the super we're currently converting.
          -- If it is then we can perform a self tail-call.
          ,  (A.XVar (C.UName nSuperCalled), _xsTys)    <- A.splitXApps xFunTys
          ,  Just nSuperCurrent                         <- contextSuperName ctx
          ,  nSuperCalled == nSuperCurrent

          -- We only do this self tailcall optimisation when all the parameters
          -- are boxed object and the Slotify transform has introduced a stack
          -- slot for each of them.
          ,  all (\t -> t == A.tPtr A.rTop A.tObj)
                 [t | A.RType t <- tsArgs]
          ,  arity == length (contextSuperParamSlots ctx)
          ,  arity == length tsArgs

          -- Lookup the label we need to jump to to re-start the super.
          ,  Just lSuperBody    <- contextSuperBodyLabel ctx

          -- Actions to convert each of the arguments.
          ,  Just msArgs        <- sequence $ map (mconvArg ctx) xsArgs
          -> do
                -- Convert each of the arguments.
                xsArgs'         <- sequence msArgs

                -- Build a tail call that writes the new arguments to the corresponding
                -- shadow stack slots and jumps back to the start of the function.
                let isCall
                        = Seq.fromList
                                [ annotNil $ IStore (XVar nDst) xSrc
                                        | nDst  <- map snd $ contextSuperParamSlots ctx
                                        | xSrc  <- xsArgs' ]
                        |> (annotNil $ IBranch lSuperBody)

                return  $ blocks
                        |> (Block label $ instrs >< isCall)


         -- Perform a non-self tailcall.
         --   This is a tail call to some other super besides the one
         --   currently being converted. Although we can avoid allocating
         --   stack space for the formal parameters, when we call the super
         --   it may allocate more space on the stack for its own shadow stack
         --   slots.
         A.XApp{}
          | Just (prim, args)                           <- A.takeXPrimApps xx
          , A.PrimControl (A.PrimControlTailCall arity) <- prim
          , arity > 0

          -- Split the arguments to the tail-call primitive.
          , _tsArgs                                     <- take arity args
          , A.RType tResult : A.RExp xFunTys : xsArgs   <- drop arity args

          -- Action to get the name of the super we're calling.
          , (xFun, _xsTys)      <- A.splitXApps xFunTys
          , Just mFun           <- takeGlobalV ctx xFun

          -- Actions to convert each of the arguments.
          , Just msArgs         <- sequence $ map (mconvArg ctx) xsArgs
          -> do
                Var nFun _      <- mFun
                xsArgs'         <- sequence msArgs
                tResult'        <- convertType pp kenv tResult

                if isVoidT tResult
                  -- Tail called function returns void.
                  then do
                        return $ blocks
                         |> (Block label $ instrs
                            |> (annotNil $ ICall Nothing CallTypeTail Nothing
                                                tResult' nFun xsArgs' [])
                            |> (annotNil $ IReturn Nothing))

                  -- Tail called function returns an actual value.
                  else do
                        vDst      <- newUniqueVar tResult'
                        return  $ blocks
                         |> (Block label $ instrs
                            |> (annotNil $ ICall (Just vDst) CallTypeTail Nothing
                                                tResult' nFun xsArgs' [])
                            |> (annotNil $ IReturn (Just (XVar vDst))))


         -- Assignment ------------------------------------
         -- Read from a pointer, with integrated bounds check.
         A.XLet (A.LLet (C.BName nDst _) x1) x2
          | Just (p, as)                         <- A.takeXPrimApps x1
          , A.PrimStore A.PrimStorePeekBounded   <- p
          , A.RType{} : A.RType tDst : args      <- as
          , Just [mPtr, mOffset, mLength]        <- atomsR args
          -> do
                tDst'           <- convertType pp kenv tDst
                (ctx', vDst@(Var nDst' _))
                                <- bindLocalV ctx nDst tDst

                xPtr'           <- mPtr
                xOffset'        <- mOffset
                xLength'        <- mLength
                let vTest       =  Var (bumpName nDst' "test")  (TInt  1)
                let vAddr1      =  Var (bumpName nDst' "addr1") (tAddr pp)
                let vAddr2      =  Var (bumpName nDst' "addr2") (tAddr pp)
                let vPtr        =  Var (bumpName nDst' "ptr")   (tPtr tDst')

                labelFail       <- newUniqueLabel "peek-bounds"
                labelOk         <- newUniqueLabel "peek-ok"

                let blockEntry  = Block label
                                $ instrs
                                >< (Seq.fromList $ map annotNil
                                [ ICmp      vTest (ICond ICondUlt) xOffset' xLength'
                                , IBranchIf (XVar vTest) labelOk labelFail ])

                let blockFail   = Block labelFail
                                $ Seq.fromList $ map annotNil
                                [ case ectx of
                                   ExpTop{}            -> INop
                                   ExpNest _   vDst' _ -> ISet vDst' (XUndef (typeOfVar vDst'))
                                   ExpAssign _ vDst'   -> ISet vDst' (XUndef (typeOfVar vDst'))

                                , ICall Nothing CallTypeStd Nothing
                                        TVoid (NameGlobal "abort") [] []

                                , IUnreachable]

                let instrsCont  = Seq.fromList $ map annotNil
                                [ IConv     vAddr1 ConvPtrtoint xPtr'
                                , IOp       vAddr2 OpAdd (XVar vAddr1) xOffset'
                                , IConv     vPtr   ConvInttoptr (XVar vAddr2)
                                , ILoad     vDst   (XVar vPtr)]

                convertSuperBody ctx' ectx
                        (blocks |> blockEntry |> blockFail)
                        labelOk instrsCont x2


         -- Write to a pointer, with integrated bounds check.
         A.XLet (A.LLet _ x1) x2
          | Just (p, as)                         <- A.takeXPrimApps x1
          , A.PrimStore A.PrimStorePokeBounded   <- p
          , A.RType{} : A.RType tVal : args      <- as
          , Just [mPtr, mOffset, mLength, mVal]  <- atomsR args
          -> do
                tVal'           <- convertType pp kenv tVal
                xPtr'           <- mPtr
                xOffset'        <- mOffset
                xLength'        <- mLength
                xVal'           <- mVal

                vTest           <- newUniqueNamedVar "test"  (TInt  1)
                vAddr1          <- newUniqueNamedVar "addr1" (tAddr pp)
                vAddr2          <- newUniqueNamedVar "addr2" (tAddr pp)
                vPtr            <- newUniqueNamedVar "ptr"   (tPtr  tVal')

                labelFail       <- newUniqueLabel "poke-bounds"
                labelOk         <- newUniqueLabel "poke-ok"

                let blockEntry  = Block label
                                $ instrs
                                >< (Seq.fromList $ map annotNil
                                [ ICmp      vTest (ICond ICondUlt) xOffset' xLength'
                                , IBranchIf (XVar vTest) labelOk labelFail ])

                let blockFail   = Block labelFail
                                $ Seq.fromList $ map annotNil
                                [ case ectx of
                                   ExpTop{}            -> INop
                                   ExpNest _   vDst' _ -> ISet vDst' (XUndef (typeOfVar vDst'))
                                   ExpAssign _ vDst'   -> ISet vDst' (XUndef (typeOfVar vDst'))

                                , ICall Nothing CallTypeStd Nothing
                                        TVoid (NameGlobal "abort") [] []

                                , IUnreachable]

                let instrsCont  = Seq.fromList $ map annotNil
                                [ IConv     vAddr1 ConvPtrtoint xPtr'
                                , IOp       vAddr2 OpAdd (XVar vAddr1) xOffset'
                                , IConv     vPtr   ConvInttoptr (XVar vAddr2)
                                , IStore    (XVar vPtr)  xVal' ]

                convertSuperBody ctx ectx
                        (blocks |> blockEntry |> blockFail)
                        labelOk instrsCont x2


         -- A let-bound expression without a name, of the void type.
         A.XLet (A.LLet (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs' <- convertSimple ctx ectx x1
                convertSuperBody ctx ectx blocks label
                        (instrs >< instrs') x2


         -- A let-bound expression without a name, of some non-void type.
         --   In C we can just drop a computed value on the floor,
         --   but the LLVM compiler needs an explicit name for it.
         --   Add the required name then call ourselves again.
         A.XLet (A.LLet (C.BNone t) x1) x2
          | not $ isVoidT t
          -> do n       <- newUnique
                let b   = C.BName (A.NameVar (T.pack $ "_d" ++ show n)) t

                convertSuperBody ctx ectx blocks label instrs
                        (A.XLet (A.LLet b x1) x2)


         -- Variable assigment from a case-expression.
         A.XLet (A.LLet (C.BName nm t)
                        (A.XCase xScrut alts))
                  x2
          -> do
                -- Bind the Salt name, allocating a new LLVM variable for it.
                --   The alternatives assign their final result to this variable.
                (ctx', vCont) <- bindLocalV ctx nm t

                -- Label to jump to continue evaluating 'x1'
                lCont         <- newUniqueLabel "cont"

                -- Convert the alternatives.
                --   As the let-binding is non-recursive, the alternatives are
                --   converted in the original context, without the let-bound
                --   variable (ctx).
                let ectx'   =  ExpNest ectx vCont lCont
                blocksCase  <- convertCase ctx ectx' label instrs xScrut alts

                -- Convert the body of the let-expression.
                --   This is done in the new context, with the let-bound variable.
                convertSuperBody ctx' ectx
                        (blocks >< blocksCase)
                        lCont Seq.empty x2


         -- Variable assignment from an instantiated super name.
         -- We can't generate LLVM code for these bindings directly, so they are
         -- stashed in the context until we find a conversion that needs them.
         -- See [Note: Binding top-level supers]
         A.XLet (A.LLet (C.BName nBind _) x1) x2
          | (xF, asArgs)                <- A.splitXApps x1
          , A.XVar (C.UName nSuper)     <- xF
          , tsArgs  <- [t | A.RType t   <- asArgs]
          , length tsArgs > 0
          , length asArgs == length tsArgs
          ,   Set.member nSuper (contextSupers  ctx)
           || Set.member nSuper (contextImports ctx)
          -> do let ctx'   = ctx { contextSuperBinds
                                    = Map.insert nBind (nSuper, tsArgs)
                                        (contextSuperBinds ctx) }
                convertSuperBody ctx' ectx blocks label instrs x2


         -- Variable assignment from some other expression.
         A.XLet (A.LLet (C.BName nm t) x1) x2
          -> do
                -- Bind the Salt name, allocating a new LLVM variable name for it.
                (ctx', vDst) <- bindLocalV ctx nm t

                -- Convert the bound expression.
                --   As the let-binding is non-recursive, the bound expression
                --   is converted in the original context, without the let-bound
                --   variable (ctx).
                instrs'  <- convertSimple ctx (ExpAssign ectx vDst) x1

                -- Convert the body of the let-expression.
                --   This is done in the new context, with the let-bound variable.
                convertSuperBody ctx' ectx blocks label (instrs >< instrs') x2


         -- Letregions ------------------------------------
         A.XLet (A.LPrivate bsType _mt _) x2
          -> do let ctx'  = extendsKindEnv bsType ctx
                convertSuperBody ctx' ectx blocks label instrs x2


         -- Case ------------------------------------------
         A.XCase xScrut alts
          -> do blocks' <- convertCase ctx ectx label instrs xScrut alts
                return  $ blocks >< blocks'


         -- Cast -------------------------------------------
         A.XCast _ x
          -> convertSuperBody ctx ectx blocks label instrs x

         _
          | ExpNest _ vDst label' <- ectx
          -> do instrs'  <- convertSimple ctx (ExpAssign ectx vDst) xx
                return  $ blocks >< Seq.singleton (Block label
                                (instrs >< (instrs' |> (annotNil $ IBranch label'))))

          |  otherwise
          -> throw $ ErrorInvalidExp xx
                   $ Just "Cannot use this as the body of a super."


-- Exp --------------------------------------------------------------------------------------------
-- | Convert a simple Core expression to LLVM instructions.
--
--   This only works for variables, literals, and full applications of
--   primitive operators. The client should ensure the program is in this form
--   before converting it. The result is just a sequence of instructions,
--   so there are no new labels to jump to.
--
convertSimple
        :: Context -> ExpContext
        -> A.Exp
        -> ConvertM (Seq AnnotInstr)

convertSimple ctx ectx xx
 = let  pp      = contextPlatform ctx
        tenv    = contextTypeEnv  ctx
        kenv    = contextKindEnv  ctx
   in do
        case xx of
         -- Atoms
         _ | ExpAssign _ vDst   <- ectx
           , Just mx            <- mconvAtom ctx xx
           -> do x' <- mx
                 return $ Seq.singleton $ annotNil
                        $ ISet vDst x'

         -- Primitive values.
         A.XPrim p
          | mDst        <- takeNonVoidVarOfContext ectx
          , Just go     <- foldl (<|>) empty
                                [ convPrimCall  ctx mDst p []
                                , convPrimArith ctx mDst p []
                                , convPrimCast  ctx mDst p []
                                , convPrimStore ctx mDst p [] ]
          -> go

         -- Primitive operators.
         A.XApp{}
          | Just (p, args) <- A.takeXPrimApps xx
          , mDst        <- takeNonVoidVarOfContext ectx
          , Just go     <- foldl (<|>) empty
                                [ convPrimCall  ctx mDst p args
                                , convPrimArith ctx mDst p args
                                , convPrimCast  ctx mDst p args
                                , convPrimStore ctx mDst p args ]
          -> go

          -- Call to top-level super.
          | (xFun@(A.XVar u), xsArgs) <- A.splitXApps xx
          , Just tSuper         <- Env.lookup u tenv
          , Just msArgs_value   <- sequence $ map (mconvArg ctx) $ eraseTypeWitArgs xsArgs
          , Just mFun           <- takeGlobalV ctx xFun
          -> do
                Var nFun _      <- mFun
                xsArgs_value'   <- sequence $ msArgs_value
                (_, tResult)    <- convertSuperType pp kenv tSuper
                let mv          = case tResult of
                                        TVoid   -> Nothing
                                        _       -> takeNonVoidVarOfContext ectx

                return  $ Seq.singleton $ annotNil
                        $ ICall  mv CallTypeStd Nothing
                                 tResult nFun xsArgs_value' []
         -- Casts
         A.XCast _ x
           -> convertSimple ctx ectx x

         _ -> throw $ ErrorInvalidExp xx
                    $ Just "Was expecting a variable, primitive, or super application."


---------------------------------------------------------------------------------------------------
-- | Erase type and witness arge Slurp out only the values from a list of
--   function arguments.
eraseTypeWitArgs :: [A.Arg] -> [A.Arg]
eraseTypeWitArgs []       = []
eraseTypeWitArgs (x:xs)
 = case x of
        A.RType{}       -> eraseTypeWitArgs xs
        A.RWitness{}    -> eraseTypeWitArgs xs
        _               -> x : eraseTypeWitArgs xs


-- | Append the given string to a name.
bumpName :: Name -> String -> Name
bumpName nn s
 = case nn of
        NameLocal str   -> NameLocal  (str ++ "." ++ s)
        NameGlobal str  -> NameGlobal (str ++ "." ++ s)
