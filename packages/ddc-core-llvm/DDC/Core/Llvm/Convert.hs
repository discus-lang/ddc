
-- | Conversion of Disciple Core-Sea to LLVM.
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Transform.Clean
import DDC.Llvm.Module
import DDC.Llvm.Function
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Prim
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Convert.Erase
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Type.Env                     (Env)
import Control.Monad.State.Strict       (evalState)
import Control.Monad.State.Strict       (gets)
import Control.Monad
import Data.Maybe
import Data.Sequence                    (Seq, (<|), (|>), (><))
import Data.Map                         (Map)
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Seq


-- Module ---------------------------------------------------------------------
-- | Convert a module to LLVM
convertModule :: Platform -> C.Module () A.Name -> Module
convertModule platform mm@(C.ModuleCore{})
 = let  prims           = primDeclsMap platform
        state           = llvmStateInit platform prims
   in   clean $ evalState (convModuleM mm) state


convModuleM :: C.Module () A.Name -> LlvmM Module
convModuleM mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   platform        <- gets llvmStatePlatform

        -- The initial environments due to imported names.
        let kenv        = C.moduleKindEnv mm
        let tenv        = C.moduleTypeEnv mm `Env.union` (Env.fromList $ map fst bxs)

        -- Forward declarations for imported functions.
        let Just importDecls 
                = sequence
                $ [ importedFunctionDeclOfType platform kenv External n t
                  | (n, t)   <- Map.elems $ C.moduleImportTypes mm ]

        -- Add RTS def -------------------------------------------------
        -- TODO: Split this into separate function.
        -- If this is the main module then we need to declare
        -- the global RTS state.
        let isMainModule 
                = C.moduleName mm == C.ModuleName ["Main"]

        let vHeapTop    = Var (NameGlobal "DDC.Runtime.heapTop")  (tAddr platform)
        let rtsGlobals
                | isMainModule
                = [ GlobalStatic   vHeapTop (StaticLit (LitInt (tAddr platform) 0)) ]

                | otherwise
                = [ GlobalExternal vHeapTop ]
        ---------------------------------------------------------------

        functions       <- mapM (uncurry (convSuperM kenv tenv)) bxs

        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = rtsGlobals
                , modFwdDecls   = primDecls platform ++ importDecls
                , modFuncs      = functions }

 | otherwise    = die "invalid module"


-- | Global variables used directly by the conversion.
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
        :: Env A.Name                   -- ^ Kind environment.
        -> Env A.Name                   -- ^ Type environment.
        -> C.Bind A.Name                -- ^ Bind for the super.
        -> C.Exp () A.Name              -- ^ Super body.
        -> LlvmM Function

convSuperM kenv tenv bSuper@(C.BName (A.NameVar nTop) tSuper) x
 | Just (bfsParam, xBody)  <- takeXLamFlags x
 = do   
        platform         <- gets llvmStatePlatform

        -- Add parameters to environments.                                      -- TODO: this scoping isn't right.
        let bsParamType  = [b | (True,  b) <- bfsParam]
        let bsParamValue = [b | (False, b) <- bfsParam]

        let kenv'       = Env.extends bsParamType  kenv
        let tenv'       = Env.extends (bSuper : bsParamValue) tenv

        -- Sanitise the super name so we can use it as a symbol
        -- in the object code.
        let nTop'       = A.sanitizeName nTop

        -- Split off the argument and result types of the super.
        let (tsParam, tResult)   
                        = convSuperType platform kenv tSuper
  
        -- Make parameter binders.
        let align       = AlignBytes (platformAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = nTop'
                , declLinkage            = External
                , declCallConv           = CC_Ccc
                , declReturnType         = tResult
                , declParamListType      = FixedArgs
                , declParams             = [Param t [] | t <- tsParam]
                , declAlign              = align }

        -- Convert function body to basic blocks.
        label   <- newUniqueLabel "entry"
        blocks  <- convBodyM kenv' tenv' Seq.empty label Seq.empty xBody

        -- Build the function.
        return  $ Function
                { funDecl               = decl
                , funParams             = map nameOfParam bsParamValue
                , funAttrs              = [] 
                , funSection            = SectionAuto
                , funBlocks             = Seq.toList blocks }

convSuperM _ _ _ _      = die "invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: C.Bind A.Name -> String
nameOfParam bb
 = case bb of
        C.BName (A.NameVar n) _ 
           -> A.sanitizeName n

        _  -> die $ "invalid parameter name: " ++ show bb


-- Body -----------------------------------------------------------------------
-- | Convert a function body to LLVM blocks.
convBodyM 
        :: Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq Instr            -- ^ Instrs in current block.
        -> C.Exp () A.Name      -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

convBodyM kenv tenv blocks label instrs xx
 = do   pp      <- gets llvmStatePlatform
        case xx of

         -- Control transfer instructions -----------------
         -- Void return applied to a literal void constructor.
         C.XApp{}
          |  Just (A.NamePrim p, xs)            <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType _, C.XCon _ dc]           <- xs
          ,  Just A.NameVoid                    <- C.takeNameOfDaCon dc
          -> return  $   blocks 
                     |>  Block label (instrs |> IReturn Nothing)

         -- Void return applied to some other expression.
         -- We still have to eval the expression, 
         --  but it returns no value.
         C.XApp{}
          |  Just (A.NamePrim p, xs)            <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType t, x2]                    <- xs
          ,  isVoidT t
          -> do instrs2 <- convStmtM pp kenv tenv x2
                return  $  blocks
                        |> Block label (instrs >< (instrs2 |> IReturn Nothing))

         -- Return a value.
         C.XApp{}
          |  Just (A.NamePrim p, xs)            <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn  <- p
          ,  [C.XType t, x]                     <- xs
          -> do let t'  =  convType pp kenv t
                vDst    <- newUniqueVar t'
                is      <- convExpM pp kenv tenv vDst x
                return  $   blocks 
                        |>  Block label (instrs >< (is |> IReturn (Just (XVar vDst))))

         -- Fail and abort the program.
         C.XApp{}
          |  Just (A.NamePrim p, xs)           <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlFail   <- p
          ,  [C.XType _tResult]                <- xs
          -> let iFail  = ICall Nothing CallTypeStd 
                                 TVoid 
                                 (NameGlobal "abort")
                                 [] []

             in  return  $   blocks 
                         |>  Block label (instrs |> iFail |> IUnreachable)


         -- Calls -----------------------------------------
         -- Tailcall a function.
         C.XApp{}
          |  Just (A.NamePrim p, args)         <- takeXPrimApps xx
          ,  A.PrimCall (A.PrimCallTail arity) <- p
          ,  _tsArgs                           <- take arity args
          ,  C.XType tResult : xFun : xsArgs   <- drop arity args
          ,  Just (Var nFun _)                 <- takeGlobalV pp kenv tenv xFun
          ,  Just xsArgs'                      <- sequence $ map (mconvAtom pp kenv tenv) xsArgs
          -> if isVoidT tResult
              -- Tailcalled function returns void.
              then do return $ blocks
                        |> (Block label $ instrs
                           |> ICall Nothing CallTypeTail
                                   (convType pp kenv tResult) nFun xsArgs' []
                           |> IReturn Nothing)

              -- Tailcalled function returns an actual value.
              else do let tResult'    = convType pp kenv tResult
                      vDst            <- newUniqueVar tResult'
                      return  $ blocks
                       |> (Block label $ instrs
                          |> ICall (Just vDst) CallTypeTail 
                                   (convType pp kenv tResult) nFun xsArgs' []
                          |> IReturn (Just (XVar vDst)))


         -- Assignment ------------------------------------
         -- Variable assignment from an expression.
         C.XLet _ (C.LLet C.LetStrict b@(C.BName (A.NameVar n) t) x1) x2
          -> do let tenv' = Env.extend b tenv
                let n'    = A.sanitizeName n

                let t'    = convType pp kenv t
                let dst   = Var (NameLocal n') t'
                instrs'   <- convExpM pp kenv tenv dst x1
                convBodyM kenv tenv' blocks label (instrs >< instrs') x2

         -- A statement that provides no value statment.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs'   <- convStmtM pp kenv tenv x1
                convBodyM kenv tenv blocks label (instrs >< instrs') x2

         -- Variable assignment from an unsed binder.
         -- We need to invent a dummy binder as LLVM needs some name for it.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          -> do let t'    =  convType pp kenv t
                dst       <- newUniqueNamedVar "dummy" t'
                instrs'   <- convExpM pp kenv tenv dst x1
                convBodyM kenv tenv blocks label (instrs >< instrs') x2

         -- Letregions
         C.XLet _ (C.LLetRegion b _) x2
          -> do let kenv' = Env.extend b kenv
                convBodyM kenv' tenv blocks label instrs x2

         -- Case statement.
         C.XCase _ x1 alts
          | Just x1'@(Var{})    <- takeLocalV pp kenv tenv x1
          -> do alts'@(_:_)     <- mapM (convAltM kenv tenv) alts

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

                let switchBlock = Block label
                                $ instrs |> ISwitch (XVar x1') lDefault table

                return  $ blocks >< (switchBlock <| (blocksTable >< blocksDefault))

         _ -> die $ "invalid body statement " ++ show xx
 

-- Stmt -----------------------------------------------------------------------
-- | Convert a Core statement to LLVM instructions.
convStmtM 
        :: Platform             -- ^ Platfom specification.
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> C.Exp () A.Name      -- ^ Expression to convert.
        -> LlvmM (Seq Instr)

convStmtM pp kenv tenv xx
 = case xx of
        -- Call to primop.
        C.XApp{}
         |  Just (C.XVar _ (C.UPrim (A.NamePrim p) tPrim), xs) <- takeXApps xx
         -> convPrimCallM pp kenv tenv Nothing p tPrim xs

        -- Call to top-level super.
         | Just (xFun@(C.XVar _ u), xsArgs) <- takeXApps xx
         , Just (Var nFun _)                <- takeGlobalV pp kenv tenv xFun
         , Just xsArgs_value'   <- sequence $ map (mconvAtom pp kenv tenv) 
                                $  eraseTypeWitArgs xsArgs
         , Just tSuper          <- Env.lookup u tenv

         -> let (_, tResult)    =  convSuperType pp kenv tSuper

            in  return $ Seq.singleton
                    $ ICall Nothing CallTypeStd 
                         tResult nFun xsArgs_value' []

        _ -> die $ "invalid statement" ++ show xx


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
        :: Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> C.Alt () A.Name      -- ^ Alternative to convert.
        -> LlvmM AltResult

convAltM kenv tenv aa
 = do   pp      <- gets llvmStatePlatform
        case aa of
         C.AAlt C.PDefault x
          -> do label   <- newUniqueLabel "default"
                blocks  <- convBodyM kenv tenv Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

         C.AAlt (C.PData u []) x
          | Just lit     <- convPatBound pp u
          -> do label   <- newUniqueLabel "alt"
                blocks  <- convBodyM kenv tenv Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

         _ -> die "invalid alternative"


-- | Convert a pattern to a LLVM literal.
convPatBound :: Platform -> C.Bound A.Name -> Maybe Lit
convPatBound pp (C.UPrim name _)
 = case name of
        A.NameBool True   -> Just $ LitInt (TInt 1) 1
        A.NameBool False  -> Just $ LitInt (TInt 1) 0
        A.NameNat  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i
        A.NameInt  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i
        A.NameWord i bits -> Just $ LitInt (TInt $ fromIntegral bits) i
        A.NameTag  i      -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i
        _                 -> Nothing

convPatBound _ _          = Nothing


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


-- Exp ------------------------------------------------------------------------
-- | Convert a Core expression to LLVM instructions.
--
--   This only works for variables, literals, and full applications of
--   primitive operators. The client should ensure the program is in this form 
--   before converting it.
convExpM
        :: Platform             -- ^ Current platform.
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> Var                  -- ^ Assign result to this var.
        -> C.Exp () A.Name      -- ^ Expression to convert.
        -> LlvmM (Seq Instr)

convExpM pp kenv tenv vDst (C.XVar _ u@(C.UName (A.NameVar n)))
 | Just t       <- Env.lookup u tenv
 = do   let n'  = A.sanitizeName n
        let t'  = convType pp kenv t
        return  $ Seq.singleton 
                $ ISet vDst (XVar (Var (NameLocal n') t'))

convExpM pp _ _ vDst (C.XCon _ dc)
 | Just n       <- C.takeNameOfDaCon dc
 = case n of
        A.NameNat i
         -> return $ Seq.singleton
                   $ ISet vDst (XLit (LitInt (tNat pp) i))

        A.NameInt  i
         -> return $ Seq.singleton 
                   $ ISet vDst (XLit (LitInt (tInt pp) i))

        A.NameWord w bits
         -> return $ Seq.singleton
                   $ ISet vDst (XLit (LitInt (TInt $ fromIntegral bits) w))

        _ -> die "invalid literal"

convExpM pp kenv tenv dst xx@C.XApp{}
        
        -- Call to primop.
        | Just (C.XVar _ (C.UPrim (A.NamePrim p) tPrim), args) <- takeXApps xx
        = convPrimCallM pp kenv tenv (Just dst) p tPrim args

        -- Call to top-level super.
        | Just (xFun@(C.XVar _ u), xsArgs) <- takeXApps xx
        , Just (Var nFun _)                <- takeGlobalV pp kenv tenv xFun
        , Just xsArgs_value'    <- sequence $ map (mconvAtom pp kenv tenv) 
                                $  eraseTypeWitArgs xsArgs
        , Just tSuper           <- Env.lookup u tenv
        = let   (_, tResult)    = convSuperType pp kenv tSuper

          in    return $ Seq.singleton
                 $ ICall (Just dst) CallTypeStd 
                         tResult nFun xsArgs_value' []

convExpM _ _ _ _ xx
        = die $ "invalid expression " ++ show xx

