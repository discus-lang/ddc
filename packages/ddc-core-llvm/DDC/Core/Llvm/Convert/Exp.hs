
module DDC.Core.Llvm.Convert.Exp
        ( Context (..)
        , convBodyM)
where
import DDC.Core.Llvm.Convert.Prim
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Metadata.Tbaa
import DDC.Core.Llvm.LlvmM
import DDC.Llvm.Syntax
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Base.Pretty                          hiding (align)
import DDC.Data.ListUtils
import Control.Monad.State.Strict               (gets)
import Control.Monad
import Data.Maybe
import Data.Sequence                            (Seq, (<|), (|>), (><))
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert          as A
import qualified DDC.Core.Exp                   as C
import qualified DDC.Type.Env                   as Env
import qualified Data.Sequence                  as Seq

---------------------------------------------------------------------------------------------------
-- | Convert a function body to LLVM blocks.
convBodyM 
        :: Context              -- ^ Context of this conversion.
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq AnnotInstr       -- ^ Instrs in current block.
        -> C.Exp () A.Name      -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

convBodyM context kenv tenv mdsup blocks label instrs xx
 = do   pp      <- gets llvmStatePlatform
        mm      <- gets llvmStateModule
        case xx of

         -- Control transfer instructions -----------------
         -- Void return applied to a literal void constructor.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  ContextTop{}                       <- context
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
          |  ContextTop{}                       <- context
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _ t, x2]                  <- xs
          ,  isVoidT t
          -> do instrs2 <- convExpM context pp kenv tenv mdsup x2
                return  $  blocks
                        |> Block label 
                                 (instrs >< (instrs2 |> (annotNil $ IReturn Nothing)))

         -- Return a value.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  ContextTop{}                       <- context
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _ t, x]                   <- xs
          -> do let t'  =  convertType pp kenv t
                vDst    <- newUniqueVar t'
                is      <- convExpM (ContextAssign context vDst) pp kenv tenv mdsup x
                return  $   blocks 
                        |>  Block label 
                                  (instrs >< (is |> (annotNil $ IReturn (Just (XVar vDst)))))

         -- Fail and abort the program.
         --   Allow this inside an expression as well as from the top level.
         C.XApp{}
          |  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlFail    <- p
          ,  [C.XType _ _tResult]               <- xs
          -> let iFail  = ICall Nothing CallTypeStd Nothing 
                                TVoid (NameGlobal "abort") [] []

                 iSet   = case context of
                           ContextTop{}           -> INop
                           ContextNest _ vDst _   -> ISet vDst (XUndef (typeOfVar vDst))
                           ContextAssign _ vDst   -> ISet vDst (XUndef (typeOfVar vDst))

                 block  = Block label
                        $ instrs |> annotNil iSet
                                 |> annotNil iFail 
                                 |> annotNil IUnreachable


             in  return  $   blocks |> block


         -- Calls -----------------------------------------
         -- Tailcall a function.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  Just (A.NamePrimOp p, args)           <- takeXPrimApps xx
          ,  A.PrimCall (A.PrimCallTail arity)     <- p
          ,  _tsArgs                               <- take arity args
          ,  C.XType _ tResult : xFunTys : xsArgs  <- drop arity args
          ,  Just (xFun, _xsTys)        <- takeXApps xFunTys
          ,  Just (Var nFun _)          <- takeGlobalV pp mm kenv tenv xFun
          ,  Just xsArgs'               <- sequence $ map (mconvAtom pp context kenv tenv) xsArgs
          -> if isVoidT tResult
              -- Tailcalled function returns void.
              then do return $ blocks
                        |> (Block label $ instrs
                           |> (annotNil $ ICall Nothing CallTypeTail Nothing
                                               (convertType pp kenv tResult) nFun xsArgs' [])
                           |> (annotNil $ IReturn Nothing))

              -- Tailcalled function returns an actual value.
              else do let tResult'    = convertType pp kenv tResult
                      vDst            <- newUniqueVar tResult'
                      return  $ blocks
                       |> (Block label $ instrs
                          |> (annotNil $ ICall (Just vDst) CallTypeTail Nothing
                                   (convertType pp kenv tResult) nFun xsArgs' [])
                          |> (annotNil $ IReturn (Just (XVar vDst))))


         -- Assignment ------------------------------------

         -- A statement of type void does not produce a value.
         C.XLet _ (C.LLet (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs'   <- convExpM context pp kenv tenv mdsup x1
                convBodyM context kenv tenv mdsup blocks label
                        (instrs >< instrs') x2

         -- A non-void let-expression.
         --   In C we can just drop a computed value on the floor, 
         --   but the LLVM compiler needs an explicit name for it.
         --   Add the required name then call ourselves again.
         C.XLet a (C.LLet (C.BNone t) x1) x2
          | not $ isVoidT t
          -> do 
                n       <- newUnique
                let b   = C.BName (A.NameVar ("_dummy" ++ show n)) t

                convBodyM context kenv tenv mdsup blocks label instrs 
                        (C.XLet a (C.LLet b x1) x2)

         -- Variable assigment from a case-expression.
         C.XLet _ (C.LLet b@(C.BName nm t) 
                            (C.XCase _ xScrut alts)) 
                  x2
          | Just n <- A.takeNameVar nm
          -> do 
                let t'    = convertType pp kenv t

                -- Assign result of case to this variable.
                let n'    = A.sanitizeName n
                let vCont = Var (NameLocal n') t'

                -- Label to jump to continue evaluating 'x1'
                lCont   <- newUniqueLabel "cont"

                let context'    = ContextNest context vCont lCont
                blocksCase      <- convCaseM context' pp kenv tenv mdsup 
                                        label instrs xScrut alts

                let tenv'       = Env.extend b tenv
                convBodyM context kenv tenv' mdsup
                        (blocks >< blocksCase) 
                        lCont
                        Seq.empty
                        x2

         -- Variable assignment from an non-case expression.
         C.XLet _ (C.LLet b@(C.BName nm t) x1) x2
          | Just n       <- A.takeNameVar nm
          -> do let tenv' = Env.extend b tenv
                let n'    = A.sanitizeName n

                let t'    = convertType pp kenv t
                let dst   = Var (NameLocal n') t'
                instrs'   <- convExpM (ContextAssign context dst) pp kenv tenv mdsup x1
                convBodyM context kenv tenv' mdsup blocks label (instrs >< instrs') x2


         -- Letregions ------------------------------------
         C.XLet _ (C.LPrivate b _mt _) x2
          -> do let kenv' = Env.extends b kenv
                convBodyM context kenv' tenv mdsup blocks label instrs x2

         -- Case ------------------------------------------
         C.XCase _ xScrut alts
          -> do blocks' <- convCaseM context pp kenv tenv mdsup 
                                label instrs xScrut alts

                return  $ blocks >< blocks'

         -- Cast -------------------------------------------
         C.XCast _ _ x
          -> convBodyM context kenv tenv mdsup blocks label instrs x

         _ 
          | ContextNest _ vDst label' <- context
          -> do instrs'  <- convExpM (ContextAssign context vDst) pp kenv tenv mdsup xx
                return  $ blocks >< Seq.singleton (Block label 
                                (instrs >< (instrs' |> (annotNil $ IBranch label'))))

          |  otherwise
          -> die $   renderIndent
                 $   text "Invalid body statement " 
                 <$> ppr xx
 

-- Exp --------------------------------------------------------------------------------------------
-- | Convert a simple Core expression to LLVM instructions.
--
--   This only works for variables, literals, and full applications of
--   primitive operators. The client should ensure the program is in this form 
--   before converting it. The result is just a sequence of instructions,
 --  so there are no new labels to jump to.
convExpM
        :: Context
        -> Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> C.Exp () A.Name      -- ^ Expression to convert.
        -> LlvmM (Seq AnnotInstr)

convExpM context pp kenv tenv mdsup xx
 = do   mm      <- gets llvmStateModule 
        case xx of
         C.XVar _ u@(C.UName nm)
          | Just t               <- Env.lookup u tenv
          , ContextAssign _ vDst <- context
          , Just n               <- A.takeNameVar nm
          -> do let n'  = A.sanitizeName n
                let t'  = convertType pp kenv t
                return  $ Seq.singleton $ annotNil
                        $ ISet vDst (XVar (Var (NameLocal n') t'))

         C.XCon _ C.DaConUnit
          | ContextAssign _ vDst <- context
          -> return $ Seq.singleton $ annotNil
                    $ ISet vDst (XLit (LitNull (TPointer (tObj pp))))

         C.XCon _ dc
          | Just n               <- takeNameOfDaCon dc
          , ContextAssign _ vDst <- context
          -> case n of
                A.NameLitNat i
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (tNat pp) i))

                A.NameLitInt  i
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (tInt pp) i))

                A.NameLitWord w bits
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (TInt $ fromIntegral bits) w))

                A.NameLitBool b
                 | i <- if b then 1 else 0
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (TInt 1) i))

                _ -> die "Invalid literal"

         C.XApp{}
          -- Call to unknown function.
          | Just (C.XVar _ (C.UPrim (A.NamePrimOp p) _tPrim), xsArgs) 
                                        <- takeXApps xx
          , A.PrimCall (A.PrimCallStd arity) <- p
          , Just (xFun' : xsArgs')      <- sequence 
                                        $  map (mconvAtom pp context kenv tenv) xsArgs
          -> do let mv  = takeNonVoidVarOfContext context

                vFun@(Var nFun _) 
                        <- newUniqueNamedVar "fun" 
                        $  TPointer $ tFunction (replicate arity (tAddr pp)) (tAddr pp)

                return  $ Seq.fromList $ map annotNil
                        [ IConv vFun (ConvInttoptr) xFun'
                        , ICall mv  CallTypeStd Nothing
                                    (tAddr pp) nFun xsArgs' []]

          -- Call to other primop.
          | Just (C.XVar _ (C.UPrim (A.NamePrimOp p) tPrim), args) <- takeXApps xx
          -> convPrimCallM pp context kenv tenv mdsup
                         (takeNonVoidVarOfContext context)
                         p tPrim args

          -- Call to top-level super.
          | Just (xFun@(C.XVar _ u), xsArgs) <- takeXApps xx
          , Just (Var nFun _)                <- takeGlobalV pp mm kenv tenv xFun
          , Just xsArgs_value'    <- sequence $ map (mconvAtom pp context kenv tenv) 
                                  $  eraseTypeWitArgs xsArgs
          , Just tSuper           <- Env.lookup u tenv
          -> let (_, tResult)   = convertSuperType pp kenv tSuper

                 mv             = case tResult of
                                        TVoid   -> Nothing
                                        _       -> takeNonVoidVarOfContext context

             in  return $ Seq.singleton $ annotNil
                        $ ICall  mv CallTypeStd Nothing
                                 tResult nFun xsArgs_value' []

         C.XCast _ _ x
          -> convExpM context pp kenv tenv mdsup x

         _ -> die $ "Invalid expression " ++ show xx


tFunction :: [Type] -> Type -> Type
tFunction tsArgs tResult
        = TFunction
        $ FunctionDecl
        { declName              = "anon"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = tResult
        , declParamListType     = FixedArgs
        , declParams            = [ Param t [] | t <- tsArgs ]
        , declAlign             = AlignNone }

-- Case -------------------------------------------------------------------------------------------
convCaseM 
        :: Context
        -> Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> Label                -- label of current block
        -> Seq AnnotInstr       -- intrs to prepend to initial block.
        -> C.Exp () A.Name
        -> [C.Alt () A.Name]
        -> LlvmM (Seq Block)

convCaseM context pp kenv tenv mdsup label instrs xScrut alts 
 | Just vScrut'@Var{}   <- takeLocalV pp kenv tenv xScrut
 = do   
        -- Convert all the alternatives.
        -- If we're in a nested context we'll also get a block to join the 
        -- results of each alternative.
        (alts', blocksJoin)
                <- convAlts context pp kenv tenv mdsup alts

        -- Build the switch ---------------
        -- Determine what default alternative to use for the instruction. 
        (lDefault, blocksDefault)
         <- case last alts' of
                AltDefault l bs -> return (l, bs)
                AltCase _  l bs -> return (l, bs)

        -- Alts that aren't the default.
        let Just altsTable = takeInit alts'

        -- Build the jump table of non-default alts.
        let table       = mapMaybe takeAltCase altsTable
        let blocksTable = join $ fmap altResultBlocks $ Seq.fromList altsTable

        let switchBlock 
                =  Block label
                $  instrs 
                |> (annotNil $ ISwitch (XVar vScrut') lDefault table)

        return  $  switchBlock 
                <| (blocksTable >< blocksDefault >< blocksJoin)

convCaseM _ _ _ _ _ _ _ _ _
        = die "Invalid case expression"


-- Alts -------------------------------------------------------------------------------------------
convAlts 
        :: Context
        -> Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> [C.Alt () A.Name]
        -> LlvmM ([AltResult], Seq Block)

-- Alternatives are at top level.
convAlts context@ContextTop{} 
         _pp kenv tenv mdsup alts
 = do   
        alts'   <- mapM (convAltM context kenv tenv mdsup) alts
        return  (alts', Seq.empty)


-- If we're doing a branch inside a let-binding we need to add a join
-- point to collect the results from each altenative before continuing
-- on to evaluate the rest.
convAlts (ContextNest ctx vDst lCont)
         _pp kenv tenv mdsup alts
 = do
        let tDst'       = typeOfVar vDst

        -- Label of the block that does the join.
        lJoin           <- newUniqueLabel "join"

        -- Convert all the alternatives,
        -- assiging their results into separate vars.
        (vDstAlts, alts'@(_:_))
                <- liftM unzip 
                $  mapM (\alt -> do
                        vDst'   <- newUniqueNamedVar "alt" tDst'
                        alt'    <- convAltM (ContextNest ctx vDst' lJoin) kenv tenv mdsup alt
                        return (vDst', alt'))
                $  alts

        -- A block to join the result from each alternative.
        --  Trying to keep track of which block a variable is defined in is 
        --  too hard when we have nested join points. 
        --  Instead, we set the label here to 'unknown' and fix this up in the
        --  Clean transform.
        let blockJoin   
                = Block lJoin
                $ Seq.fromList $ map annotNil
                [ IPhi vDst  [ (XVar vDstAlt, Label "unknown")
                             | vDstAlt   <- vDstAlts ]
                , IBranch lCont ]

        return (alts', Seq.singleton blockJoin)

convAlts (ContextAssign{}) _ _ _ _ _
 = die "Cannot convert alts in this context."


-- Alt --------------------------------------------------------------------------------------------
-- | Holds the result of converting an alternative.
data AltResult
        = AltDefault        Label (Seq Block)
        | AltCase       Lit Label (Seq Block)


-- | Convert a case alternative to LLVM.
--
--   This only works for zero-arity constructors.
--   The client should extrac the fields of algebraic data objects manually.
convAltM 
        :: Context              -- ^ Context we're converting in.
        -> KindEnv  A.Name      -- ^ Kind environment.
        -> TypeEnv  A.Name      -- ^ Type environment.
        -> MDSuper              -- ^ Meta-data for the enclosing super.
        -> C.Alt () A.Name      -- ^ Alternative to convert.
        -> LlvmM AltResult

convAltM context kenv tenv mdsup aa
 = do   pp      <- gets llvmStatePlatform
        case aa of
         C.AAlt C.PDefault x
          -> do label   <- newUniqueLabel "default"
                blocks  <- convBodyM context kenv tenv mdsup Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

         C.AAlt (C.PData C.DaConUnit []) x
          -> do label   <- newUniqueLabel "alt"
                blocks  <- convBodyM context kenv tenv mdsup Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

         C.AAlt (C.PData dc []) x
          | Just n      <- takeNameOfDaCon dc
          , Just lit    <- convPatName pp n
          -> do label   <- newUniqueLabel "alt"
                blocks  <- convBodyM context kenv tenv mdsup Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

         _ -> die "Invalid alternative"


-- | Convert a constructor name from a pattern to a LLVM literal.
--
--   Only integral-ish types can be used as patterns, for others 
--   such as Floats we rely on the Lite transform to have expanded
--   cases on float literals into a sequence of boolean checks.
convPatName :: Platform -> A.Name -> Maybe Lit
convPatName pp name
 = case name of
        A.NameLitBool True   -> Just $ LitInt (TInt 1) 1
        A.NameLitBool False  -> Just $ LitInt (TInt 1) 0

        A.NameLitNat  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.NameLitInt  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i

        A.NameLitWord i bits 
         | elem bits [8, 16, 32, 64]
         -> Just $ LitInt (TInt $ fromIntegral bits) i

        A.NameLitTag  i      -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i

        _                    -> Nothing


-- | Take the blocks from an `AltResult`.
altResultBlocks :: AltResult -> Seq Block
altResultBlocks aa
 = case aa of
        AltDefault _ blocks     -> blocks
        AltCase _ _  blocks     -> blocks


-- | Take the `Lit` and `Label` from an `AltResult`
takeAltCase :: AltResult -> Maybe (Lit, Label)
takeAltCase (AltCase lit label _)       = Just (lit, label)
takeAltCase _                           = Nothing

