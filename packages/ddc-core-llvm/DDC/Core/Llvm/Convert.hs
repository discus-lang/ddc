
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
import DDC.Core.Llvm.LlvmM
import DDC.Core.Salt.Platform
import DDC.Core.Salt.Sanitize
import DDC.Core.Compounds
import DDC.Type.Compounds
import Data.Sequence                    (Seq, (<|), (|>), (><))
import Data.Map                         (Map)
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Seq
import Control.Monad.State.Strict       (evalState)
import Control.Monad.State.Strict       (gets)
import Control.Monad
import Data.Maybe


-- Module ---------------------------------------------------------------------
-- | Convert a module to LLVM
convertModule :: Platform -> C.Module () A.Name -> Module
convertModule platform mm
 = let  prims           = primDeclsMap platform
        state           = llvmStateInit platform prims
   in   clean $ evalState (convModuleM mm) state


convModuleM :: C.Module () A.Name -> LlvmM Module
convModuleM mm@(C.ModuleCore{})
 | ([C.LRec bxs], _)    <- splitXLets $ C.moduleBody mm
 = do   platform        <- gets llvmStatePlatform

        -- Forward declarations for imported functions.
        let Just importDecls 
                = sequence
                $ [ importedFunctionDeclOfType platform External n t
                  | (n, t)   <- Map.elems $ C.moduleImportTypes mm ]


        functions       <- mapM (uncurry (convSuperM)) bxs
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = []
                , modFwdDecls   = primDecls platform ++ importDecls
                , modFuncs      = functions }

 | otherwise    = die "invalid module"


primDeclsMap :: Platform -> Map String FunctionDecl
primDeclsMap pp
        = Map.fromList
        $ [ (declName decl, decl) | decl <- primDecls pp ]


-- | Global variables used directly by the conversion.
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
convSuperM 
        :: C.Bind A.Name                -- ^ Bind for the super.
        -> C.Exp () A.Name              -- ^ Super body.
        -> LlvmM Function

convSuperM (C.BName (A.NameVar nTop) tSuper) x
 | Just (bsParam, xBody)  <- takeXLams x
 = do   platform          <- gets llvmStatePlatform

        let nTop' = sanitizeName nTop

        -- Split off the argument and result types.
        let (tsArgs, tResult)       
                  = takeTFunArgResult tSuper

        -- Make parameter binders.
        let params      = map (llvmParameterOfType platform) tsArgs
        let align       = AlignBytes (platformAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = nTop'
                , declLinkage            = External
                , declCallConv           = CC_Ccc
                , declReturnType         = convType platform tResult
                , declParamListType      = FixedArgs
                , declParams             = params
                , declAlign              = align }

        -- Convert function body to basic blocks.
        label   <- newUniqueLabel "entry"
        blocks  <- convBodyM Seq.empty label Seq.empty xBody

        -- Build the function.
        return  $ Function
                { funDecl               = decl
                , funParams             = map nameOfParam bsParam
                , funAttrs              = [] 
                , funSection            = SectionAuto
                , funBlocks             = Seq.toList blocks }

convSuperM _ _          = die "invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: C.Bind A.Name -> String
nameOfParam bb
 = case bb of
        C.BName (A.NameVar n) _ 
           -> sanitizeName n

        _  -> die "invalid parameter name"


-- Body -----------------------------------------------------------------------
-- | Convert a function body to LLVM blocks.
convBodyM 
        :: Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq Instr            -- ^ Instrs in current block.
        -> C.Exp () A.Name      -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

convBodyM blocks label instrs xx
 = do   pp      <- gets llvmStatePlatform
        case xx of

         -- End of function body must explicitly pass control.
         C.XApp{}
          |  Just (A.NamePrim p, xs)             <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn   <- p
          ,  [C.XType _, C.XCon _ (C.UPrim n _)] <- xs
          ,  A.NameVoid                          <- n
          -> return  $   blocks 
                     |>  Block label (instrs |> IReturn Nothing)

         C.XApp{}
          |  Just (A.NamePrim p, xs)           <- takeXPrimApps xx
          ,  A.PrimControl A.PrimControlReturn <- p
          ,  [C.XType t, x]                    <- xs
          -> do let t'  =  convType pp t
                vDst    <- newUniqueVar t'
                is      <- convExpM pp vDst x
                return  $   blocks 
                        |>  Block label (instrs >< (is |> IReturn (Just (XVar vDst))))

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


         -- Variable assignment.
         C.XLet _ (C.LLet C.LetStrict (C.BName (A.NameVar n) t) x1) x2
          -> do t'       <- convTypeM t
                let n'   = sanitizeName n
                let dst  = Var (NameLocal n') t'
                instrs'  <- convExpM pp dst x1
                convBodyM blocks label (instrs >< instrs') x2

         -- Non-binding statment.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs'   <- convStmtM pp x1
                convBodyM blocks label (instrs >< instrs')   x2

         -- Case statement.
         C.XCase _ x1 alts
          | Just x1'@(Var{})    <- takeLocalV pp x1
          -> do alts'@(_:_)     <- mapM convAltM alts

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
convStmtM :: Platform -> C.Exp () A.Name -> LlvmM (Seq Instr)
convStmtM pp xx
 = case xx of
        -- Call to primop.
        C.XApp{}
         |  C.XVar _ (C.UPrim (A.NamePrim p) tPrim) : xs <- takeXApps xx
         -> convPrimCallM pp Nothing p tPrim xs

        -- Call to top-level super.
          | xFun@(C.XVar _ b) : xsArgs    <- takeXApps xx
          , Just (Var nFun _)             <- takeGlobalV pp xFun
          , (_, tResult)                  <- takeTFunArgResult $ typeOfBound b
          , Just xsArgs'                  <- sequence $ map (mconvAtom pp) xsArgs
          -> return $ Seq.singleton
                    $ ICall Nothing CallTypeStd 
                         (convType pp tResult) nFun xsArgs' []

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
convAltM :: C.Alt () A.Name -> LlvmM AltResult
convAltM aa
 = do   pp      <- gets llvmStatePlatform
        case aa of
         C.AAlt C.PDefault x
          -> do label   <- newUniqueLabel "default"
                blocks  <- convBodyM Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

         C.AAlt (C.PData u []) x
          | Just lit     <- convPatBound pp u
          -> do label   <- newUniqueLabel "alt"
                blocks  <- convBodyM Seq.empty label Seq.empty x
                return  $  AltCase lit label blocks

         _ -> die "invalid alternative"


-- | Convert a pattern to a LLVM literal.
convPatBound :: Platform -> C.Bound A.Name -> Maybe Lit
convPatBound pp (C.UPrim name _)
 = case name of
        A.NameTag  i      -> Just $ LitInt (TInt (8 * platformTagBytes pp))  i
        A.NameNat  i      -> Just $ LitInt (TInt (8 * platformAddrBytes pp)) i
        A.NameInt  i bits -> Just $ LitInt (TInt $ fromIntegral bits) i
        A.NameWord i bits -> Just $ LitInt (TInt $ fromIntegral bits) i
        A.NameBool True   -> Just $ LitInt (TInt 1) 1
        A.NameBool False  -> Just $ LitInt (TInt 1) 0
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
        -> Var                  -- ^ Assign result to this var.
        -> C.Exp () A.Name      -- ^ Expression to convert.
        -> LlvmM (Seq Instr)

convExpM _  vDst (C.XVar _ (C.UName (A.NameVar n) t))
 = do   let n'  = sanitizeName n
        t'      <- convTypeM t
        return  $ Seq.singleton 
                $ ISet vDst (XVar (Var (NameLocal n') t'))


convExpM pp vDst (C.XCon _ (C.UPrim name _t))
 = case name of
        A.NameNat i
         -> return $ Seq.singleton
                   $ ISet vDst (XLit (LitInt (tNat pp) i))

        A.NameWord w bits
         -> return $ Seq.singleton
                   $ ISet vDst (XLit (LitInt (TInt $ fromIntegral bits) w))

        A.NameInt  w bits
         -> return $ Seq.singleton 
                   $ ISet vDst (XLit (LitInt (TInt $ fromIntegral bits) w))

        _ -> die "invalid literal"

convExpM pp dst xx@C.XApp{}
        
        -- Call to primop.
        | (C.XVar _ (C.UPrim (A.NamePrim p) tPrim) : args) 
                <- takeXApps xx
        = convPrimCallM pp (Just dst) p tPrim args

        -- Call to top-level super.
        | xFun@(C.XVar _ b) : xsArgs    <- takeXApps xx
        , Just (Var nFun _)             <- takeGlobalV pp xFun
        , (_, tResult)                  <- takeTFunArgResult $ typeOfBound b
        , Just xsArgs'                  <- sequence $ map (mconvAtom pp) xsArgs
        = return $ Seq.singleton
                 $ ICall (Just dst) CallTypeStd 
                         (convType pp tResult) nFun xsArgs' []

convExpM _ _ _
        = die "invalid expression"


