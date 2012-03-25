
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Instr
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Core.Llvm.Convert.Type
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
          -> do altResults      <- mapM convAltM alts

                let altTable    = mapMaybe takeAltCase altResults
                let altBlocks   = join $ fmap altResultBlocks $ Seq.fromList altResults

                let switchBlock = Block label
                                $ instrs |> ISwitch (XVar x1') (Label "foo") altTable

                return  $ switchBlock <| altBlocks


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
        | Just (E.NamePrim p, args)     <- takeXPrimApps xx
        = convPrimCallM pp (Just dst) p args

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
         |  C.XVar _ (C.UPrim (E.NamePrim p) _) : xs <- takeXApps xx
         -> convPrimCallM pp Nothing p xs

        _ -> error "convStmtM: sorry"


-- Prim call ------------------------------------------------------------------
-- | Convert a primitive call to LLVM.
convPrimCallM 
        :: Show a 
        => Platform             -- ^ Current platform.
        -> Maybe Var            -- ^ Assign result to this var.
        -> E.Prim               -- ^ Prim to call.
        -> [C.Exp a E.Name]     -- ^ Arguments to prim.
        -> LlvmM (Seq Instr)

convPrimCallM pp mdst p xs
 = case p of
        E.PrimOp op
         | [C.XType t, x1, x2] <- xs
         , t'           <- convType    pp t
         , Just x1'     <- mconvAtom   pp x1
         , Just x2'     <- mconvAtom   pp x2
         , Just op'     <- convPrimOp2 op t
         , Just dst     <- mdst
         -> return      $ Seq.singleton
                        $ IOp dst op' t' x1' x2'


        E.PrimStore E.PrimStoreAlloc
         | [xBytes]     <- xs
         , Just xBytes' <- mconvAtom pp xBytes
         -> return      $ Seq.singleton
                        $ ICall mdst CallTypeStd
                                (tPtr (TInt 8)) (NameGlobal "malloc") 
                                [xBytes'] []


        E.PrimExternal (E.PrimExternalShowInt bitsInt)
         |  [xVal]      <- xs
         ,  Just val    <- mconvAtom pp xVal
         -> return      $ Seq.singleton
                        $ ICall mdst
                                CallTypeStd 
                                (tPtr (TInt 8)) 
                                (NameGlobal $ "showInt" ++ show bitsInt)
                                [val] []

        _ -> return $ Seq.singleton 
           $ IComment ["convPrimCallM: cannot convert " ++ show (p, xs)]


-- | Convert a binary primop from Core Sea to LLVM form.
convPrimOp2 :: E.PrimOp -> C.Type E.Name -> Maybe Op
convPrimOp2 op t
 = case op of
        E.PrimOpAdd     -> Just OpAdd
        E.PrimOpSub     -> Just OpSub
        E.PrimOpMul     -> Just OpMul

        E.PrimOpDiv
         | Just True    <- isSignedT t  
         -> Just OpSDiv

         | otherwise
         -> Just OpUDiv

        E.PrimOpMod
         | Just True    <- isSignedT t
         -> Just OpSRem

         | otherwise    
         -> Just OpURem

        _               -> Nothing


isSignedT :: C.Type E.Name -> Maybe Bool
isSignedT tt
 = case tt of
        -- A primitive type.
        C.TCon (C.TyConBound (C.UPrim (E.NamePrimTyCon n) _))
         -> case n of
                E.PrimTyConNat          -> Just False
                E.PrimTyConWord  _      -> Just False
                E.PrimTyConInt   _      -> Just True
                E.PrimTyConFloat _      -> Just True
                _                       -> Nothing

        _                               -> Nothing


-- Atoms ----------------------------------------------------------------------
-- | Take a variable or literal from an expression.
--   These can be used directly in instructions.
mconvAtom :: Platform -> C.Exp a E.Name -> Maybe Exp
mconvAtom pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar str) t)
         -> Just $ XVar (Var (NameLocal str) (convType pp t))

        C.XCon _ (C.UPrim (E.NameTag  tag) t)
         -> Just $ XLit (LitInt (convType pp t) tag)

        C.XCon _ (C.UPrim (E.NameNat  nat) t)
         -> Just $ XLit (LitInt (convType pp t) nat)

        C.XCon _ (C.UPrim (E.NameInt  val _) t)
         -> Just $ XLit (LitInt (convType pp t) val)

        C.XCon _ (C.UPrim (E.NameWord val _) t)
         -> Just $ XLit (LitInt (convType pp t) val)

        _ -> Nothing


-- Utils ----------------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  :: Platform -> C.Exp a E.Name -> Maybe Var
takeLocalV pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar str) t)
          -> Just $ Var (NameLocal str) (convType pp t)
        _ -> Nothing


-- | Take a variable from an expression as a local var, if any.
takeGlobalV  :: Platform -> C.Exp a E.Name -> Maybe Var
takeGlobalV pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar str) t)
          -> Just $ Var (NameGlobal str) (convType pp t)
        _ -> Nothing

