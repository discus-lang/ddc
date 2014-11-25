
module DDC.Core.Llvm.Convert.Exp
        ( Context (..)
        , convertBody
        , convertExp)
where
import DDC.Core.Llvm.Convert.Exp.PrimCall
import DDC.Core.Llvm.Convert.Exp.PrimArith
import DDC.Core.Llvm.Convert.Exp.PrimCast
import DDC.Core.Llvm.Convert.Exp.PrimStore
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Exp.Base
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Compounds
import Control.Applicative
import Data.Sequence                            (Seq, (|>), (><))
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert          as A
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
        -> C.Exp () A.Name      -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

convertBody ctx ectx blocks label instrs xx
 = let  pp           = contextPlatform    ctx 
        kenv         = contextKindEnv     ctx
        convertCase  = contextConvertCase ctx
   in do   
        case xx of

         -- Control transfer instructions -----------------
         -- Void return applied to a literal void constructor.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType{}, C.XCon _ dc]           <- xs
          ,  Just A.NameLitVoid                 <- takeNameOfDaCon dc
          -> return  $   blocks 
                     |>  Block label 
                               (instrs |> (annotNil $ IReturn Nothing))

         -- Void return applied to some other expression.
         --   We still have to eval the expression, but it returns no value.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _ t, x2]                  <- xs
          ,  isVoidT t
          -> do instrs2 <- convertExp ctx ectx x2
                return  $  blocks
                        |> Block label 
                                 (instrs >< (instrs2 |> (annotNil $ IReturn Nothing)))

         -- Return a value.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  ExpTop{}                           <- ectx
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _ t, x]                   <- xs
          -> do let t'  =  convertType pp kenv t
                vDst    <- newUniqueVar t'
                is      <- convertExp ctx (ExpAssign ectx vDst) x
                return  $   blocks 
                        |>  Block label 
                                  (instrs >< (is |> (annotNil $ IReturn (Just (XVar vDst)))))

         -- Fail and abort the program.
         --   Allow this inside an expression as well as from the top level.
         C.XApp{}
          |  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlFail    <- p
          ,  [C.XType _ _tResult]               <- xs
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
         C.XApp{}
          |  Just (A.NamePrimOp p, args)          <- takeXPrimApps xx
          ,  A.PrimCall (A.PrimCallTail arity)    <- p
          ,  _tsArgs                              <- take arity args
          ,  C.XType _ tResult : xFunTys : xsArgs <- drop arity args
          ,  Just (xFun, _xsTys)                  <- takeXApps xFunTys
          ,  Just (Var nFun _)                    <- takeGlobalV ctx xFun
          ,  Just xsArgs'                         <- sequence $ map (mconvAtom ctx) xsArgs
          -> if isVoidT tResult
              -- Tail called function returns void.
              then do return $ blocks
                        |> (Block label $ instrs
                           |> (annotNil $ ICall Nothing CallTypeTail Nothing
                                               (convertType pp kenv tResult) nFun xsArgs' [])
                           |> (annotNil $ IReturn Nothing))

              -- Tail called function returns an actual value.
              else do let tResult'    = convertType pp kenv tResult
                      vDst            <- newUniqueVar tResult'
                      return  $ blocks
                       |> (Block label $ instrs
                          |> (annotNil $ ICall (Just vDst) CallTypeTail Nothing
                                   (convertType pp kenv tResult) nFun xsArgs' [])
                          |> (annotNil $ IReturn (Just (XVar vDst))))


         -- Assignment ------------------------------------
         -- A let-bound expression without a name, of the void type.
         C.XLet _ (C.LLet (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs' <- convertExp ctx ectx x1
                convertBody ctx ectx blocks label
                        (instrs >< instrs') x2


         -- A let-bound expression without a name, of some non-void type.
         --   In C we can just drop a computed value on the floor, 
         --   but the LLVM compiler needs an explicit name for it.
         --   Add the required name then call ourselves again.
         C.XLet a (C.LLet (C.BNone t) x1) x2
          | not $ isVoidT t
          -> do n       <- newUnique
                let b   = C.BName (A.NameVar ("_dummy" ++ show n)) t

                convertBody ctx ectx blocks label instrs 
                        (C.XLet a (C.LLet b x1) x2)


         -- Variable assigment from a case-expression.
         C.XLet _ (C.LLet b@(C.BName nm t) 
                            (C.XCase _ xScrut alts)) 
                  x2
          | Just n <- A.takeNameVar nm
          -> do 
                let t'      = convertType pp kenv t

                -- Assign result of case to this variable.
                let n'      = A.sanitizeName n
                let vCont   = Var (NameLocal n') t'

                -- Label to jump to continue evaluating 'x1'
                lCont       <- newUniqueLabel "cont"
                let ectx'   =  ExpNest ectx vCont lCont
                blocksCase  <- convertCase ctx ectx' label instrs xScrut alts

                let ctx'    = extendTypeEnv b ctx
                convertBody ctx' ectx
                        (blocks >< blocksCase) 
                        lCont
                        Seq.empty
                        x2


         -- Variable assignment from an instantiated super name.
         -- We can't generate LLVM code for these bindings directly, so they are
         -- stashed in the context until we find a conversion that needs them.
         -- See [Note: Binding top-level supers]
         C.XLet _ (C.LLet (C.BName nBind _) x1) x2
          | Just (xF, xsArgs)         <- takeXApps x1
          , C.XVar _ (C.UName nSuper) <- xF
          , atsArgs     <- [(a, t) | C.XType a t <- xsArgs]
          , tsArgs      <- map snd atsArgs
          , length tsArgs > 0
          , length xsArgs == length tsArgs
          ,   Set.member nSuper (contextSupers  ctx)
           || Set.member nSuper (contextImports ctx)
          -> do let ctx'   = ctx { contextSuperBinds 
                                    = Map.insert nBind (nSuper, tsArgs)
                                        (contextSuperBinds ctx) }
                convertBody ctx' ectx blocks label instrs x2 


         -- Variable assignment from some other expression.
         C.XLet _ (C.LLet b@(C.BName nm t) x1) x2
          | Just n       <- A.takeNameVar nm
          -> do let n'    = A.sanitizeName n
                let t'    = convertType pp kenv t
                let dst   = Var (NameLocal n') t'
                instrs'   <- convertExp ctx (ExpAssign ectx dst) x1
                
                let ctx'  = extendTypeEnv b ctx
                convertBody ctx' ectx blocks label (instrs >< instrs') x2


         -- Letregions ------------------------------------
         C.XLet _ (C.LPrivate bsType _mt _) x2
          -> do let ctx'  = extendsKindEnv bsType ctx
                convertBody ctx' ectx blocks label instrs x2


         -- Case ------------------------------------------
         C.XCase _ xScrut alts
          -> do blocks' <- convertCase ctx ectx label instrs xScrut alts
                return  $ blocks >< blocks'


         -- Cast -------------------------------------------
         C.XCast _ _ x
          -> convertBody ctx ectx blocks label instrs x

         _ 
          | ExpNest _ vDst label' <- ectx
          -> do instrs'  <- convertExp ctx (ExpAssign ectx vDst) xx
                return  $ blocks >< Seq.singleton (Block label 
                                (instrs >< (instrs' |> (annotNil $ IBranch label'))))

          |  otherwise
          -> die $     P.renderIndent
                 $     P.text "Invalid body statement " 
                 P.<$> P.ppr xx
 

-- Exp --------------------------------------------------------------------------------------------
-- | Convert a simple Core expression to LLVM instructions.
--
--   This only works for variables, literals, and full applications of
--   primitive operators. The client should ensure the program is in this form 
--   before converting it. The result is just a sequence of instructions,
--   so there are no new labels to jump to.
--
convertExp
        :: Context -> ExpContext
        -> C.Exp () A.Name
        -> LlvmM (Seq AnnotInstr)

convertExp ctx ectx xx
 = let  pp      = contextPlatform ctx
        tenv    = contextTypeEnv  ctx
        kenv    = contextKindEnv  ctx
   in do   
        case xx of
         -- Atoms
         _ | ExpAssign _ vDst   <- ectx
           , Just x'            <- mconvAtom ctx xx
           -> return    $ Seq.singleton $ annotNil 
                        $ ISet vDst x'


         -- Primitive operators.
         C.XApp{}
          | Just (C.XVar _ (C.UPrim (A.NamePrimOp p) tPrim), args) <- takeXApps xx
          , mDst        <- takeNonVoidVarOfContext ectx
          , Just go     <- foldl (<|>) empty
                                [ convPrimCall  ctx mDst p tPrim args
                                , convPrimArith ctx mDst p tPrim args
                                , convPrimCast  ctx mDst p tPrim args
                                , convPrimStore ctx mDst p tPrim args ]
          -> go


          -- Call to top-level super.
          | Just (xFun@(C.XVar _ u), xsArgs) <- takeXApps xx
          , Just (Var nFun _)     <- takeGlobalV ctx xFun
          , Just xsArgs_value'    <- sequence $ map (mconvAtom ctx) 
                                  $  eraseTypeWitArgs xsArgs
          , Just tSuper           <- Env.lookup u tenv
          -> let (_, tResult)   = convertSuperType pp kenv tSuper

                 mv             = case tResult of
                                        TVoid   -> Nothing
                                        _       -> takeNonVoidVarOfContext ectx

             in  return $ Seq.singleton $ annotNil
                        $ ICall  mv CallTypeStd Nothing
                                 tResult nFun xsArgs_value' []

         -- Casts
         C.XCast _ _ x
          -> convertExp ctx ectx x


         _ -> die $ "Invalid expression " ++ show xx

