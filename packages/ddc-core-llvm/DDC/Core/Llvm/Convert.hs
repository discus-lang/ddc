
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Instr
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Core.Llvm.Convert.Prim
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Atom
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Compounds
import DDC.Type.Compounds
import Data.Sequence                            (Seq, (<|), (|>), (><))
import Data.Map                                 (Map)
import qualified DDC.Core.Sea.Output.Name       as E
import qualified DDC.Core.Sea.Output.Env        as E
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified DDC.Base.Pretty                as P
import qualified Data.Map                       as Map
import qualified Data.Sequence                  as Seq
import qualified Data.Foldable                  as Seq
import Control.Monad.State.Strict               (evalState)
import Control.Monad.State.Strict               (gets)
import Control.Monad
import Data.Maybe


-- Module ---------------------------------------------------------------------
convertModule :: C.Module () E.Name -> Module
convertModule mm
 = let  platform        = platform32
        prims           = primGlobals platform
        state           = llvmStateInit platform prims
   in   evalState (llvmOfModuleM mm) state


llvmOfModuleM 
        :: C.Module () E.Name 
        -> LlvmM Module

llvmOfModuleM mm@(C.ModuleCore{})
 | [C.LRec bxs]         <- C.moduleLets mm   
 = do   platform        <- gets llvmStatePlatform
        functions       <- mapM (uncurry (convSuperM)) bxs
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = functions }

 | otherwise    = die "invalid module"


primGlobals :: Platform -> Map String Var
primGlobals platform
        = Map.fromList
        [ ( "malloc"
          , Var (NameGlobal "malloc")
                (convType platform (E.tNat `tFunPE` E.tPtr E.tObj))) ]


-- Super ----------------------------------------------------------------------
-- | Convert a top-level supercombinator to LLVM.
convSuperM 
        :: C.Bind E.Name 
        -> C.Exp () E.Name 
        -> LlvmM Function

convSuperM (C.BName n tSuper) x
 | Just (bsParam, xBody)     <- takeXLams x
 = do   platform        <- gets llvmStatePlatform

        -- Split off the argument and result types.
        let (tsArgs, tResult)       
                = takeTFunArgResult tSuper

        -- Make parameter binders.
        let params      = map (llvmParameterOfType platform) tsArgs
        let align       = AlignBytes (platformAlignBytes platform)

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = P.renderPlain $ P.ppr n
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
                { functionDecl           = decl
                , functionParams         = map nameOfParam bsParam
                , functionAttrs          = [] 
                , functionSection        = SectionAuto
                , functionBlocks         = Seq.toList blocks }

convSuperM _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
nameOfParam :: C.Bind E.Name -> String
nameOfParam bb
 = case bb of
        C.BName n _     -> P.renderPlain $ P.ppr n
        _               -> die "invalid parameter name"


-- Body -----------------------------------------------------------------------
-- | Convert a Core function body to LLVM blocks.
convBodyM 
        :: Seq Block            -- ^ Previous blocks.
        -> Label                -- ^ Id of current block.
        -> Seq Instr            -- ^ Instrs in current block.
        -> C.Exp () E.Name      -- ^ Expression being converted.
        -> LlvmM (Seq Block)    -- ^ Final blocks of function body.

convBodyM blocks label instrs xx
 = do   platform        <- gets llvmStatePlatform
        case xx of

         -- End of function body must explicitly pass control.
         C.XApp{}
          |  Just (E.NamePrim p, xs)           <- takeXPrimApps xx
          ,  E.PrimControl E.PrimControlReturn <- p
          ,  [C.XType _t, x]                   <- xs
          ,  Just x'                           <- mconvAtom platform x
          -> return  $   blocks 
                     |>  Block label (instrs |> IReturn (Just x'))

         -- Variable assignment.
         C.XLet _ (C.LLet C.LetStrict (C.BName (E.NameVar str) t) x1) x2
          -> do t'       <- convTypeM t
                let dst  = Var (NameLocal str) t'
                instrs'  <- convExpM platform dst x1
                convBodyM blocks label (instrs >< instrs') x2

         -- Non-binding statment.
         C.XLet _ (C.LLet C.LetStrict (C.BNone t) x1) x2
          | isVoidT t
          -> do instrs'   <- convStmtM platform x1
                convBodyM blocks label (instrs >< instrs')   x2

         -- Case statement.
         C.XCase _ x1 alts
          | Just x1'@(Var{})    <- takeLocalV platform x1
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


         -- TODO: Debugging
         _ -> return $  blocks
                     |> Block label (instrs |> IComment [show xx])

         -- die "invalid body statement"


-- | Check whether this is the Void# type.
isVoidT :: C.Type E.Name -> Bool
isVoidT (C.TCon (C.TyConBound (C.UPrim (E.NamePrimTyCon E.PrimTyConVoid) _))) = True
isVoidT _ = False


-- Alt ------------------------------------------------------------------------
-- | Holds the result of converting an alternative.
data AltResult 
        = AltDefault        Label (Seq Block)
        | AltCase       Lit Label (Seq Block)

convAltM :: C.Alt () E.Name -> LlvmM AltResult
convAltM aa
 = case aa of
        C.AAlt C.PDefault x
         -> do  label   <- newUniqueLabel "default"
                blocks  <- convBodyM Seq.empty label Seq.empty x
                return  $  AltDefault label blocks

        C.AAlt (C.PData u []) x
         -> do  label   <- newUniqueLabel "alt"
                blocks  <- convBodyM Seq.empty label Seq.empty x
                let lit =  convPatBound u
                return  $  AltCase lit label blocks

        _ -> error "convAltM: sorry"


convPatBound :: C.Bound E.Name -> Lit           -- TODO: finish me
convPatBound _
        = LitUndef (TInt 32)


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
-- Convert a Core expression to LLVM instructions.
convExpM
        :: Platform             -- ^ Current platform.
        -> Var                  -- ^ Assign result to this var.
        -> C.Exp () E.Name      -- ^ Expression to convert.
        -> LlvmM (Seq Instr)

convExpM _  dst (C.XVar _ (C.UName (E.NameVar str) t))
 = do   t'      <- convTypeM t
        return  $ Seq.singleton 
                $ ISet dst (XVar (Var (NameLocal str) t'))

convExpM pp dst xx@C.XApp{}

        -- Call to primop.
        | (C.XVar _ (C.UPrim (E.NamePrim p) tPrim) : args) 
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

convExpM _ _ xx
        = return $ Seq.singleton 
        $ IComment [ "convExpM: cannot convert " ++ show xx ]


-- Stmt -----------------------------------------------------------------------
convStmtM :: Platform -> C.Exp () E.Name -> LlvmM (Seq Instr)
convStmtM pp xx
 = case xx of
        C.XApp{}
         |  C.XVar _ (C.UPrim (E.NamePrim p) tPrim) : xs <- takeXApps xx
         -> convPrimCallM pp Nothing p tPrim xs

        _ -> error "convStmtM: sorry"




