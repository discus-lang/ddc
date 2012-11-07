
-- | Conversion of Disciple Core-Sea to LLVM.
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Module
import DDC.Llvm.Function
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Prim
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.Convert.Metadata.Tbaa
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Type.Env                     (KindEnv, TypeEnv)
import DDC.Type.Predicates
import DDC.Base.Pretty                  hiding (align)
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import qualified DDC.Type.Env           as Env
import qualified DDC.Core.Simplifier    as Simp
import qualified DDC.Llvm.Transform.Clean as Clean
import Control.Monad.State.Strict       (evalState)
import Control.Monad.State.Strict       (gets)
import Control.Monad
import Data.Maybe
import Data.Sequence                    (Seq, (<|), (|>), (><))
import Data.Map                         (Map)
import Data.Set                         (Set)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Seq


-- Module ---------------------------------------------------------------------
-- | Convert a module to LLVM.
convertModule :: Platform -> C.Module () A.Name -> Module
convertModule platform mm@(C.ModuleCore{})
 = let  
        prims   = primDeclsMap platform
        state   = llvmStateInit platform prims

        -- Add extra Const and Distinct witnesses where possible.
        --  This helps us produce better LLVM metat data.
        mmElab   = evalState (Simp.applyTransform A.profile Env.empty Env.empty 
                                        Simp.Elaborate mm) 
                          state

        -- Convert to LLVM.
        --  The result contains ISet and INop meta instructions that need to be 
        --  cleaned out. We also need to fixup the labels in IPhi instructions.
        mmRaw    = evalState (convModuleM mmElab) state

        -- Clean out the ISet and INop meta instructions, and fixup IPhis.
        --  This gives us code that the LLVM compiler will accept directly.
        mmClean  = Clean.clean mmRaw mmRaw

   in   mmClean


convModuleM :: C.Module () A.Name -> LlvmM Module
convModuleM mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   platform        <- gets llvmStatePlatform

        -- The initial environments due to imported names.
        let kenv        = C.moduleKindEnv mm
        let tenv        = C.moduleTypeEnv mm `Env.union` (Env.fromList $ map fst bxs)

        -- Names of exported functions.
        --   We use a different linkage for exported functions.
        let nsExports   = Set.fromList $ Map.keys $ C.moduleExportTypes mm

        -- Forward declarations for imported functions.
        let Just importDecls 
                = sequence
                $ [ importedFunctionDeclOfType platform kenv External n t
                  | (n, t)   <- Map.elems $ C.moduleImportTypes mm ]

        -- Add RTS def -------------------------------------------------
        -- If this is the main module then we need to declare
        -- the global RTS state.
        let isMainModule 
                = C.moduleName mm == C.ModuleName ["Main"]

        -- Holds the pointer to the current top of the heap.
        --  This is the byte _after_ the last byte used by an object.
        let vHeapTop    = Var (NameGlobal "_DDC_Runtime_heapTop") (tAddr platform)

        -- Holds the pointer to the maximum heap.
        --  This is the byte _after_ the last byte avaiable in the heap.
        let vHeapMax    = Var (NameGlobal "_DDC_Runtime_heapMax") (tAddr platform)

        let rtsGlobals
                | isMainModule
                = [ GlobalStatic   vHeapTop (StaticLit (LitInt (tAddr platform) 0))
                  , GlobalStatic   vHeapMax (StaticLit (LitInt (tAddr platform) 0)) ]

                | otherwise
                = [ GlobalExternal vHeapTop 
                  , GlobalExternal vHeapMax ]

        ---------------------------------------------------------------
        (functions, mdecls)
                <- liftM unzip 
                $ mapM (uncurry (convSuperM nsExports kenv tenv)) bxs
        
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = rtsGlobals
                , modFwdDecls   = primDecls platform ++ importDecls 
                , modFuncs      = functions 
                , modMDecls     = concat mdecls }

 | otherwise    = die "invalid module"


-- | Global variables used directly by the converted code.
primDeclsMap :: Platform -> Map String FunctionDecl
primDeclsMap pp 
        = Map.fromList
        $ [ (declName decl, decl) | decl <- primDecls pp ]

primDecls :: Platform -> [FunctionDecl]
primDecls pp 
 = [    FunctionDecl
        { declName              = "malloc"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = tAddr pp
        , declParamListType     = FixedArgs
        , declParams            = [Param (tNat pp) []]
        , declAlign             = AlignBytes (platformAlignBytes pp) }

   ,    FunctionDecl
        { declName              = "abort"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = TVoid
        , declParamListType     = FixedArgs
        , declParams            = []
        , declAlign             = AlignBytes (platformAlignBytes pp) } ]


-- Super ----------------------------------------------------------------------
-- | Convert a top-level supercombinator to a LLVM function.
--   Region variables are completely stripped out.
convSuperM 
        :: Set A.Name           -- ^ Names exported from this module.
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Bind  A.Name       -- ^ Bind of the top-level super.
        -> C.Exp () A.Name      -- ^ Super body.
        -> LlvmM (Function, [MDecl])

convSuperM nsExports kenv tenv bSuper@(C.BName nTop@(A.NameVar strTop) tSuper) x
 | Just (bfsParam, xBody)  <- takeXLamFlags x
 = do   
        platform         <- gets llvmStatePlatform

        -- Sanitise the super name so we can use it as a symbol
        -- in the object code.
        let nTop'       = A.sanitizeGlobal strTop

        -- Add parameters to environments.
        let bfsParam'    = eraseWitBinds bfsParam
        let bsParamType  = [b | (True,  b) <- bfsParam']
        let bsParamValue = [b | (False, b) <- bfsParam']

        let kenv'       =  Env.extends bsParamType  kenv
        let tenv'       =  Env.extends (bSuper : bsParamValue) tenv
        mdsup           <- deriveMD nTop' x

        -- Split off the argument and result types of the super.
        let (tsParam, tResult)   
                        = convSuperType platform kenv tSuper
  
        -- Make parameter binders.
        let align       = AlignBytes (platformAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = nTop'

                  -- Set internal linkage for non-exported functions so that they
                  -- they won't conflict with functions of the same name that
                  -- might be defined in other modules.
                , declLinkage
                        = if Set.member nTop nsExports
                                then External
                                else Internal

                  -- ISSUE #266: Tailcall optimisation doesn't work for exported functions.
                  --   Using fast calls for non-exported functions enables the
                  --   LLVM tailcall optimisation. We can't enable this for exported
                  --   functions as well because we don't distinguish between DDC
                  --   generated functions and functions from the C libararies in 
                  --   our import specifications. We need a proper FFI system so that
                  --   we can get tailcalls for exported functions as well.
                , declCallConv           
                        = if Set.member nTop nsExports
                                then CC_Ccc
                                else CC_Fastcc

                , declReturnType         = tResult
                , declParamListType      = FixedArgs
                , declParams             = [Param t [] | t <- tsParam]
                , declAlign              = align }

        -- Convert function body to basic blocks.
        label   <- newUniqueLabel "entry"
        blocks  <- convBodyM BodyTop kenv' tenv' mdsup Seq.empty label Seq.empty xBody

        -- Build the function.
        return  $ ( Function
                    { funDecl     = decl
                    , funParams   = map nameOfParam $ filter (not . isBNone) bsParamValue
                    , funAttrs    = [] 
                    , funSection  = SectionAuto
                    , funBlocks   = Seq.toList blocks }
                  , decls mdsup )
                  

convSuperM _ _ _ _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: C.Bind A.Name -> String
nameOfParam bb
 = case bb of
        C.BName (A.NameVar n) _ 
           -> A.sanitizeName n

        _  -> die $ "invalid parameter name: " ++ show bb


-- Body -----------------------------------------------------------------------
-- | What context we're doing this conversion in.
data BodyContext
        -- | Conversion at the top-level of a function.
        --   The expresison being converted must eventually pass control.
        = BodyTop

        -- | In a nested context, like in the right of a let-binding.
        --   The expression should produce a value that we assign to this
        --   variable, then jump to the provided label to continue evaluation.
        | BodyNest Var Label
        deriving Show


-- | Convert a function body to LLVM blocks.
convBodyM 
        :: BodyContext          -- ^ Context of this conversion.
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
        case xx of

         -- Control transfer instructions -----------------
         -- Void return applied to a literal void constructor.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  BodyTop                            <- context
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _, C.XCon _ dc]           <- xs
          ,  Just A.NameVoid                    <- C.takeNameOfDaCon dc
          -> return  $   blocks 
                     |>  Block label 
                               (instrs |> (annotNil $ IReturn Nothing))

         -- Void return applied to some other expression.
         --   We still have to eval the expression, but it returns no value.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  BodyTop                            <- context
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType t, x2]                    <- xs
          ,  isVoidT t
          -> do instrs2 <- convExpM ExpTop pp kenv tenv mdsup x2
                return  $  blocks
                        |> Block label 
                                 (instrs >< (instrs2 |> (annotNil $ IReturn Nothing)))

         -- Return a value.
         --   We must be at the top-level of the function.
         C.XApp{}
          |  BodyTop                            <- context
          ,  Just (A.NamePrimOp p, xs)          <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType t, x]                     <- xs
          -> do let t'  =  convType pp kenv t
                vDst    <- newUniqueVar t'
                is      <- convExpM (ExpAssign vDst) pp kenv tenv mdsup x
                return  $   blocks 
                        |>  Block label 
                                  (instrs >< (is |> (annotNil $ IReturn (Just (XVar vDst)))))

         -- Fail and abort the program.
         --   Allow this inside an expression as well as from the top level.
         C.XApp{}
          |  Just (A.NamePrimOp p, xs)         <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlFail   <- p
          ,  [C.XType _tResult]                <- xs
          -> let iFail  = ICall Nothing CallTypeStd Nothing 
                                TVoid (NameGlobal "abort") [] []

                 iSet   = case context of
                                BodyTop         -> INop
                                BodyNest vDst _ -> ISet vDst (XUndef (typeOfVar vDst))

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
          ,  C.XType tResult : xFunTys : xsArgs    <- drop arity args
          ,  Just (xFun, _xsTys)        <- takeXApps xFunTys
          ,  Just (Var nFun _)          <- takeGlobalV pp kenv tenv xFun
          ,  Just xsArgs'               <- sequence $ map (mconvAtom pp kenv tenv) xsArgs
          -> if isVoidT tResult
              -- Tailcalled function returns void.
              then do return $ blocks
                        |> (Block label $ instrs
                           |> (annotNil $ ICall Nothing CallTypeTail Nothing
                                               (convType pp kenv tResult) nFun xsArgs' [])
                           |> (annotNil $ IReturn Nothing))

              -- Tailcalled function returns an actual value.
              else do let tResult'    = convType pp kenv tResult
                      vDst            <- newUniqueVar tResult'
                      return  $ blocks
                       |> (Block label $ instrs
                          |> (annotNil $ ICall (Just vDst) CallTypeTail Nothing
                                   (convType pp kenv tResult) nFun xsArgs' [])
                          |> (annotNil $ IReturn (Just (XVar vDst))))


         -- Assignment ------------------------------------
         -- Variable assigment from a case-expression.
         C.XLet _ (C.LLet C.LetStrict b@(C.BName (A.NameVar n) t) 
                                        (C.XCase _ xScrut alts)) 
                  x2
          -> do 
                let t'    = convType pp kenv t

                -- Assign result of case to this variable.
                let n'    = A.sanitizeName n
                let vCont = Var (NameLocal n') t'

                -- Label to jump to continue evaluating 'x1'
                lCont   <- newUniqueLabel "cont"

                let context'    = BodyNest vCont lCont
                blocksCase      <- convCaseM context' pp kenv tenv mdsup 
                                        label instrs xScrut alts

                let tenv'       = Env.extend b tenv
                convBodyM context kenv tenv' mdsup
                        (blocks >< blocksCase) 
                        lCont
                        Seq.empty
                        x2

         -- Variable assignment from an expression.
         C.XLet _ (C.LLet C.LetStrict b@(C.BName (A.NameVar n) t) x1) x2
          -> do let tenv' = Env.extend b tenv
                let n'    = A.sanitizeName n

                let t'    = convType pp kenv t
                let dst   = Var (NameLocal n') t'
                instrs'   <- convExpM (ExpAssign dst) pp kenv tenv mdsup x1
                convBodyM context kenv tenv' mdsup blocks label (instrs >< instrs') x2

         -- A statement that provides no value.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs'   <- convExpM ExpTop pp kenv tenv mdsup x1
                convBodyM context kenv tenv mdsup blocks label (instrs >< instrs') x2

         -- Variable assignment from an unsed binder.
         -- We need to invent a dummy binder as LLVM needs some name for it.
         --  TODO: do an initial pass to add binders,
         --        so we don't need to handle this separately.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          -> do let t'    =  convType pp kenv t
                dst       <- newUniqueNamedVar "dummy" t'
                instrs'   <- convExpM (ExpAssign dst) pp kenv tenv mdsup x1
                convBodyM context kenv tenv mdsup blocks label (instrs >< instrs') x2

         -- Letregions ------------------------------------
         C.XLet _ (C.LLetRegions b _) x2
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
          | BodyNest vDst label' <- context
          -> do instrs'  <- convExpM (ExpAssign vDst) pp kenv tenv mdsup xx
                return  $ blocks >< Seq.singleton (Block label 
                                (instrs >< (instrs' |> (annotNil $ IBranch label'))))

          |  otherwise
          -> die $   renderIndent
                 $   text "Invalid body statement " 
                 <$> ppr xx
 
-- Exp ------------------------------------------------------------------------
-- | What context we're doing this conversion in.
data ExpContext
        -- | Conversion at the top-level of the function.
        --   We don't have a variable to assign the result to, 
        --    so this must be a statement that transfers control
        = ExpTop        

        -- | Conversion in a context that expects a value.
        --   We evaluate the expression and assign the result to this variable.
        | ExpAssign Var
        deriving Show


-- | Take any assignable variable from an `ExpContext`.
varOfExpContext :: ExpContext -> Maybe Var
varOfExpContext xc
 = case xc of
        ExpTop          -> Nothing
        ExpAssign var   -> Just var


-- | Convert a simple Core expression to LLVM instructions.
--
--   This only works for variables, literals, and full applications of
--   primitive operators. The client should ensure the program is in this form 
--   before converting it. The result is just a sequence of instructions,
 --  so there are no new labels to jump to.
convExpM
        :: ExpContext
        -> Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> C.Exp () A.Name      -- ^ Expression to convert.
        -> LlvmM (Seq AnnotInstr)

convExpM context pp kenv tenv mdsup xx
 = case xx of
        C.XVar _ u@(C.UName (A.NameVar n))
         | Just t               <- Env.lookup u tenv
         , ExpAssign vDst       <- context
         -> do  let n'  = A.sanitizeName n
                let t'  = convType pp kenv t
                return  $ Seq.singleton $ annotNil
                        $ ISet vDst (XVar (Var (NameLocal n') t'))
        
        C.XCon _ dc
         | Just n               <- C.takeNameOfDaCon dc
         , ExpAssign vDst       <- context
         -> case n of
                A.NameNat i
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (tNat pp) i))

                A.NameInt  i
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (tInt pp) i))

                A.NameWord w bits
                 -> return $ Seq.singleton $ annotNil
                           $ ISet vDst (XLit (LitInt (TInt $ fromIntegral bits) w))

                _ -> die "invalid literal"

        C.XApp{}
         -- Call to primop.
         | Just (C.XVar _ (C.UPrim (A.NamePrimOp p) tPrim), args) <- takeXApps xx
         -> convPrimCallM pp kenv tenv mdsup
                        (varOfExpContext context)
                        p tPrim args

         -- Call to top-level super.
         | Just (xFun@(C.XVar _ u), xsArgs) <- takeXApps xx
         , Just (Var nFun _)                <- takeGlobalV pp kenv tenv xFun
         , Just xsArgs_value'    <- sequence $ map (mconvAtom pp kenv tenv) 
                                 $  eraseTypeWitArgs xsArgs
         , Just tSuper           <- Env.lookup u tenv
         -> let (_, tResult)    = convSuperType pp kenv tSuper
            in  return $ Seq.singleton $ annotNil
                       $ ICall  (varOfExpContext context) CallTypeStd Nothing
                                tResult nFun xsArgs_value' []

        C.XCast _ _ x
         -> convExpM context pp kenv tenv mdsup x

        _ -> die $ "invalid expression " ++ show xx


-- Case -----------------------------------------------------------------------
convCaseM 
        :: BodyContext
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
        let altsTable   = init alts'

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
        = error "convCaseM: sorry"


-- Alts -----------------------------------------------------------------------
convAlts 
        :: BodyContext
        -> Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> MDSuper
        -> [C.Alt () A.Name]
        -> LlvmM ([AltResult], Seq Block)

-- Alternatives are at top level.
convAlts BodyTop 
         _pp kenv tenv mdsup alts
 = do   
        alts'   <- mapM (convAltM BodyTop kenv tenv mdsup) alts
        return  (alts', Seq.empty)


-- If we're doing a branch inside a let-binding we need to add a join
-- point to collect the results from each altenative before continuing
-- on to evaluate the rest.
convAlts (BodyNest vDst lCont)
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
                        alt'    <- convAltM (BodyNest vDst' lJoin) kenv tenv mdsup alt
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


-- Alt ------------------------------------------------------------------------
-- | Holds the result of converting an alternative.
data AltResult 
        = AltDefault        Label (Seq Block)
        | AltCase       Lit Label (Seq Block)


-- | Convert a case alternative to LLVM.
--
--   This only works for zero-arity constructors.
--   The client should extrac the fields of algebraic data objects manually.
convAltM 
        :: BodyContext
        -> KindEnv  A.Name
        -> TypeEnv  A.Name
        -> MDSuper
        -> C.Alt () A.Name      -- ^ Alternative to convert.
        -> LlvmM AltResult

convAltM context kenv tenv mdsup aa
 = do   pp      <- gets llvmStatePlatform
        case aa of
         C.AAlt C.PDefault x
          -> do label   <- newUniqueLabel "default"
                blocks  <- convBodyM context kenv tenv mdsup Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

         C.AAlt (C.PData dc []) x
          | Just n      <- C.takeNameOfDaCon dc
          , Just lit    <- convPatName pp n
          -> do label   <- newUniqueLabel "alt"
                blocks  <- convBodyM context kenv tenv mdsup Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

         _ -> die "invalid alternative"


-- | Convert a constructor name from a pattern to a LLVM literal.
convPatName :: Platform -> A.Name -> Maybe Lit
convPatName pp name
 = case name of
        A.NameBool True   -> Just $ LitInt (TInt 1) 1
        A.NameBool False  -> Just $ LitInt (TInt 1) 0
        A.NameNat  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i
        A.NameInt  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i
        A.NameWord i bits -> Just $ LitInt (TInt $ fromIntegral bits) i
        A.NameTag  i      -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i
        _                 -> Nothing


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


