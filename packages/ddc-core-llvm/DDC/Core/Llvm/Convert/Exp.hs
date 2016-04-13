
module DDC.Core.Llvm.Convert.Exp
        ( Context (..)
        , convertBody
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
import DDC.Core.Generic.Compounds
import Control.Applicative
import Data.Sequence                            (Seq, (|>), (><))
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Env                   as Env
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- | Convert a function body to LLVM blocks.
convertBody
        :: Context
        -> ExpContext
        -> Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq AnnotInstr       -- ^ Instrs in current block.
        -> A.Exp                -- ^ Expression being converted.
        -> ConvertM (Seq Block) -- ^ Final blocks of function body.

convertBody ctx ectx blocks label instrs xx
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
          ,  Just (p, as)                       <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [A.RType{}, A.RExp (A.XCon dc)]    <- as
          ,  Just (A.NamePrimLit A.PrimLitVoid) <- takeNameOfDaCon dc
          -> return  $   blocks 
                     |>  Block label 
                               (instrs |> (annotNil $ IReturn Nothing))

         -- Void return applied to some other expression.
         --   We still have to eval the expression, but it returns no value.
         --   We must be at the top-level of the function.
         A.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (p, as)                       <- takeXPrimApps xx
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
          ,  Just (p, as)                       <- takeXPrimApps xx
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
          |  Just (p, as)                       <- takeXPrimApps xx
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
         -- Tailcall a function.
         --   We must be at the top-level of the function.
         A.XApp{}
          |  Just (p, args)                     <- takeXPrimApps xx
          ,  A.PrimCall (A.PrimCallTail arity)  <- p
          ,  _tsArgs                            <- take arity args
          ,  A.RType tResult : A.RExp xFunTys : xsArgs 
                                                <- drop arity args
          ,  (xFun, _xsTys)                     <- splitXApps xFunTys
          ,  Just mFun                          <- takeGlobalV ctx xFun
          ,  Just msArgs                        <- sequence $ map (mconvArg ctx) xsArgs
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
          | Just (p, as)                         <- takeXPrimApps x1
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

                convertBody ctx' ectx 
                        (blocks |> blockEntry |> blockFail) 
                        labelOk instrsCont x2


         -- Write to a pointer, with integrated bounds check.
         A.XLet (A.LLet _ x1) x2
          | Just (p, as)                         <- takeXPrimApps x1
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

                convertBody ctx ectx
                        (blocks |> blockEntry |> blockFail) 
                        labelOk instrsCont x2


         -- A let-bound expression without a name, of the void type.
         A.XLet (A.LLet (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs' <- convertSimple ctx ectx x1
                convertBody ctx ectx blocks label
                        (instrs >< instrs') x2


         -- A let-bound expression without a name, of some non-void type.
         --   In C we can just drop a computed value on the floor, 
         --   but the LLVM compiler needs an explicit name for it.
         --   Add the required name then call ourselves again.
         A.XLet (A.LLet (C.BNone t) x1) x2
          | not $ isVoidT t
          -> do n       <- newUnique
                let b   = C.BName (A.NameVar ("_d" ++ show n)) t

                convertBody ctx ectx blocks label instrs 
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
                convertBody ctx' ectx
                        (blocks >< blocksCase) 
                        lCont Seq.empty x2


         -- Variable assignment from an instantiated super name.
         -- We can't generate LLVM code for these bindings directly, so they are
         -- stashed in the context until we find a conversion that needs them.
         -- See [Note: Binding top-level supers]
         A.XLet (A.LLet (C.BName nBind _) x1) x2
          | (xF, asArgs)                <- splitXApps x1
          , A.XVar (C.UName nSuper)     <- xF
          , tsArgs  <- [t | A.RType t   <- asArgs]
          , length tsArgs > 0
          , length asArgs == length tsArgs
          ,   Set.member nSuper (contextSupers  ctx)
           || Set.member nSuper (contextImports ctx)
          -> do let ctx'   = ctx { contextSuperBinds 
                                    = Map.insert nBind (nSuper, tsArgs)
                                        (contextSuperBinds ctx) }
                convertBody ctx' ectx blocks label instrs x2 


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
                convertBody ctx' ectx blocks label (instrs >< instrs') x2


         -- Letregions ------------------------------------
         A.XLet (A.LPrivate bsType _mt _) x2
          -> do let ctx'  = extendsKindEnv bsType ctx
                convertBody ctx' ectx blocks label instrs x2


         -- Case ------------------------------------------
         A.XCase xScrut alts
          -> do blocks' <- convertCase ctx ectx label instrs xScrut alts
                return  $ blocks >< blocks'


         -- Cast -------------------------------------------
         A.XCast _ x
          -> convertBody ctx ectx blocks label instrs x

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

         -- Primitive operators.
         A.XApp{}
          | Just (p, args) <- takeXPrimApps xx
          , mDst        <- takeNonVoidVarOfContext ectx
          , Just go     <- foldl (<|>) empty
                                [ convPrimCall  ctx mDst p args
                                , convPrimArith ctx mDst p args
                                , convPrimCast  ctx mDst p args
                                , convPrimStore ctx mDst p args ]
          -> go

          -- Call to top-level super.
          | (xFun@(A.XVar u), xsArgs) <- splitXApps xx
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
