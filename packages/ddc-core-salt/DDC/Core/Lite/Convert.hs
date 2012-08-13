
-- | Conversion of Disciple Lite to Disciple Salt.
--
module DDC.Core.Lite.Convert
        ( toSalt
        , Error(..))
where
import DDC.Core.Lite.Convert.Data
import DDC.Core.Lite.Convert.Type
import DDC.Core.Lite.Convert.Base
import DDC.Core.Salt.Convert.Init
import DDC.Core.Salt.Platform
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Check.Monad              (throw, result)
import DDC.Core.Check                    (AnTEC(..))
import DDC.Type.Env                      (Env)
import qualified DDC.Core.Lite.Name      as L
import qualified DDC.Core.Salt.Runtime   as S
import qualified DDC.Core.Salt.Name      as S
import qualified DDC.Core.Salt.Compounds as S
import qualified DDC.Type.Env            as Env
import qualified Data.Map                as Map
import Control.Monad
import Data.Maybe


-- | Convert a Disciple Core Lite module to Disciple Core Salt.
--
--   Case expressions on alrebraic data values are converted into ones that just
--   check the tag, while data constructors are unfolded into explicit allocation
--   and field initialization primops. 
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      have type annotations on every bound variable and constructor
--      a-normalised
--      If not then `Error`.
--
--   The output code contains:
--      debruijn indices.
--       these which need to be eliminated before it will pass the Salt fragment checks.
--
--   TODO: Add the alternatives that force and follow lazy thunks and indirections.
--   TODO: Expand partial and over-applications into code that explicitly builds
--         and applies thunks.
--
toSalt
        :: Show a
        => Platform                             -- ^ Platform specification.
        -> S.Config                             -- ^ Runtime configuration.
        -> DataDefs L.Name                      -- ^ Data type definitions.
        -> Env L.Name                           -- ^ Kind environment.
        -> Env L.Name                           -- ^ Type environment.
        -> Module (AnTEC a L.Name) L.Name       -- ^ Lite module to convert.
        -> Either (Error a) (Module a S.Name)   -- ^ Salt module.
toSalt platform runConfig defs kenv tenv mm
 = result $ convertM platform runConfig defs kenv tenv mm


-- Module ---------------------------------------------------------------------
convertM 
        :: Show a
        => Platform
        -> S.Config
        -> DataDefs L.Name
        -> Env L.Name
        -> Env L.Name
        -> Module (AnTEC a L.Name) L.Name 
        -> ConvertM a (Module a S.Name)

convertM pp runConfig defs kenv tenv mm
  = do  
        -- Collect up signatures of imported functions.
        tsImports'
                <- liftM Map.fromList
                $  mapM convertImportM  
                $  Map.toList
                $  moduleImportTypes mm

        -- Convert the body of the module to Salt.
        let ntsImports  = [(BName n t) | (n, (_, t)) <- Map.toList $ moduleImportTypes mm]
        let tenv'       = Env.extends ntsImports tenv
        x'              <- convertBodyX pp defs kenv tenv' $ moduleBody mm

        -- Build the output module.
        let mm_salt 
                = ModuleCore
                { moduleName           = moduleName mm
                , moduleExportKinds    = Map.empty
                , moduleExportTypes    = Map.empty
                , moduleImportKinds    = S.runtimeImportKinds
                , moduleImportTypes    = Map.union S.runtimeImportTypes tsImports'
                , moduleBody           = x' }

        -- If this is the 'Main' module then add code to initialise the 
        -- runtime system. This will fail if given a Main module with no
        -- 'main' function.
        mm_init <- case initRuntime runConfig mm_salt of
                        Nothing   -> throw ErrorMainHasNoMain
                        Just mm'  -> return mm'

        return $ mm_init
                

-- | Convert an import spec.
convertImportM
        :: (L.Name, (QualName L.Name, Type L.Name))
        -> ConvertM a (S.Name, (QualName S.Name, Type S.Name))

convertImportM (n, (qn, t))
 = do   n'      <- convertBindNameM n
        qn'     <- convertQualNameM qn
        t'      <- convertT Env.empty t
        return  (n', (qn', t'))


-- | Convert a qualified name.
convertQualNameM
        :: QualName L.Name 
        -> ConvertM a (QualName S.Name)

convertQualNameM (QualName mn n)
 = do   n'      <- convertBindNameM n
        return  $ QualName mn n'


-- Exp -------------------------------------------------------------------------
-- | Convert the body of a supercombinator to Salt.
convertBodyX 
        :: Show a 
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> Env L.Name                   -- ^ Kind environment.
        -> Env L.Name                   -- ^ Type environment.
        -> Exp (AnTEC a L.Name) L.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a S.Name)

convertBodyX pp defs kenv tenv xx
 = case xx of
        XVar _ UIx{}
         -> throw $ ErrorMalformed 
         $  "convertBodyX: can't convert program with anonymous value binders."

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertU u
                return  $  XVar a' u'

        XCon a u
         -> do  let a'  = annotTail a
                xx'     <- convertCtor pp defs kenv tenv a' u
                return  xx'

        -- Keep region and data type lambdas, 
        -- but ditch the others.
        XLAM a b x
         |   (isRegionKind $ typeOfBind b)
          || (isDataKind   $ typeOfBind b)
         -> do  let a'    =  annotTail a
                b'        <- convertB kenv b

                let kenv' =  Env.extend b kenv
                x'        <- convertBodyX pp defs kenv' tenv x

                return $ XLAM a' b' x'

         | otherwise
         -> do  let kenv'       = Env.extend b kenv
                convertBodyX pp defs kenv' tenv x


        XLam a b x
         -> let tenv'   = Env.extend b tenv
            in case universeFromType1 kenv (typeOfBind b) of
                Just UniverseData    
                 -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertB kenv b) 
                        (convertBodyX pp defs kenv tenv' x)

                Just UniverseWitness 
                 -> liftM3 XLam
                        (return $ annotTail a)
                        (convertB kenv b)
                        (convertBodyX pp defs kenv tenv' x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b

        XApp{}
         ->     convertSimpleX pp defs kenv tenv xx

        XLet a (LRec bxs) x2
         -> do  let tenv'       = Env.extends (map fst bxs) tenv
                let (bs, xs)    = unzip bxs
                bs'             <- mapM (convertB kenv) bs
                xs'             <- mapM (convertBodyX pp defs kenv tenv') xs
                x2'             <- convertBodyX pp defs kenv tenv' x2
                return $ XLet (annotTail a) (LRec $ zip bs' xs') x2'

        XLet a (LLet LetStrict b x1) x2
         -> do  let tenv'       = Env.extend b tenv
                b'              <- convertB kenv b
                x1'             <- convertSimpleX pp defs kenv tenv' x1
                x2'             <- convertBodyX   pp defs kenv tenv' x2
                return  $ XLet (annotTail a) (LLet LetStrict b' x1') x2'

        XLet _ (LLet LetLazy{} _ _) _
         -> error "DDC.Core.Lite.Convert.toSaltX: XLet lazy not handled yet"

        XLet a (LLetRegion b bs) x2
         -> do  b'              <- convertB kenv b

                let kenv'       = Env.extend b kenv
                bs'             <- mapM (convertB kenv') bs
                x2'             <- convertBodyX pp defs kenv' tenv x2
                return  $ XLet (annotTail a) (LLetRegion b' bs') x2'

        XLet _ LWithRegion{} _
         -> throw $ ErrorMalformed "LWithRegion should not appear in Lite code."


        -- Match against literal unboxed values.
        --  The branch is against the literal value itself.
        --
        --  TODO: We can't branch against float literals.
        --        Matches against float literals should be desugared into if-then-else chains.
        --        Same for string literals.
        --
        XCase (AnTEC _ _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon (TyConBound (UPrim nType _) _)  <- tScrut
         , L.NamePrimTyCon _                    <- nType
         -> do  xScrut' <- convertSimpleX pp defs kenv tenv xScrut
                alts'   <- mapM (convertAlt pp defs kenv tenv a' uScrut tScrut) alts
                return  $  XCase a' xScrut' alts'

        -- Match against finite algebraic data.
        --   The branch is against the constructor tag.
        XCase (AnTEC tX _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon (TyConBound (UPrim nType _) _) : _ <- takeTApps tScrut
         , L.NameDataTyCon _                       <- nType
         -> do  x'      <- convertSimpleX   pp defs kenv tenv xScrut
                tX'     <- convertT kenv tX
                alts'   <- mapM (convertAlt pp defs kenv tenv a' uScrut tScrut) alts

                let asDefault
                        | any isPDefault [p | AAlt p _ <- alts]   
                        = []

                        | otherwise     
                        = [AAlt PDefault (S.xFail a' tX')]

                tScrut'    <- convertT kenv tScrut
                let tPrime = fromMaybe S.rTop
                           $ takePrimeRegion tScrut'

                return  $ XCase a' (S.xGetTag a' tPrime x') 
                        $ alts' ++ asDefault

        XCase{}         -> throw $ ErrorNotNormalized ("cannot convert case expression")
        XCast _ _ x     -> convertBodyX pp defs kenv tenv x

        XType _         -> throw $ ErrorMistyped xx
        XWitness{}      -> throw $ ErrorMistyped xx


-------------------------------------------------------------------------------
-- | Convert the right of an internal let-binding.
--   The right of the binding must be straight-line code, 
--   and cannot contain case-expressions, or construct new functions.
--  
convertSimpleX
        :: Show a 
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> Env L.Name                   -- ^ Kind environment.
        -> Env L.Name                   -- ^ Type environment.
        -> Exp (AnTEC a L.Name) L.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a S.Name)

convertSimpleX pp defs kenv tenv xx
 = let downAtomX    = convertAtomX    pp defs kenv tenv
       downCtorAppX = convertCtorAppX pp defs kenv tenv
   in  case xx of

        XType{}         
         -> throw $ ErrorMalformed 
         $ "convertRValueX: XType should not appear as the right of a let-binding"

        XWitness{}
         -> throw $ ErrorMalformed 
         $ "convertRValueX: XWithess should not appear as the right of a let-binding"

        -- Data constructors.
        XApp a xa xb
         | (x1, xsArgs)           <- takeXApps' xa xb
         , XCon _ dc <- x1
         -> downCtorAppX a dc xsArgs

        -- Primitive operations.
        XApp a xa xb
         | (x1, xsArgs)          <- takeXApps' xa xb
         , XVar _ UPrim{}        <- x1
         -> do  x1'     <- downAtomX x1
                xsArgs' <- mapM downAtomX xsArgs

                return $ makeXApps (annotTail a) x1' xsArgs'

        -- Function application
        -- TODO: This only works for full application. 
        --       At least check for the other cases.
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, xsArgs) <- takeXApps' xa xb
         -> do  x1'     <- downAtomX x1
                xsArgs' <- mapM downAtomX xsArgs
                return  $ makeXApps a' x1' xsArgs'

        _ -> downAtomX xx


-------------------------------------------------------------------------------
-- | Convert an atom to Salt.
convertAtomX
        :: Show a 
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> Env L.Name                   -- ^ Kind environment.
        -> Env L.Name                   -- ^ Type environment.
        -> Exp (AnTEC a L.Name) L.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a S.Name)

convertAtomX pp defs kenv tenv xx
 = case xx of
        XVar _ UIx{}    -> throw $ ErrorMalformed     "Found anonymous binder"
        XApp{}          -> throw $ ErrorNotNormalized "Found XApp in atom position"
        XLAM{}          -> throw $ ErrorNotNormalized "Found XLAM in atom position"
        XLam{}          -> throw $ ErrorNotNormalized "Found XLam in atom position"
        XLet{}          -> throw $ ErrorNotNormalized "Found XLet in atom position"
        XCase{}         -> throw $ ErrorNotNormalized "Found XCase in atom position"

        XVar a u        
         -> do  u'  <- convertU u
                return $ XVar (annotTail a) u'

        XCon a dc       -> convertCtorAppX pp defs kenv tenv a dc []

        XCast _ _ x     -> convertAtomX    pp defs kenv tenv x

        -- Pass region parameters, as well data data type parameters to primops.
        XType t
         -> do  t'      <- convertT kenv t
                return  $ XType t'

        XWitness w      -> liftM XWitness (convertWitnessX kenv w)


-------------------------------------------------------------------------------
-- | Convert a witness expression to Salt
convertWitnessX
        :: Show a
        => Env L.Name                   -- ^ Kind enviornment
        -> Witness L.Name               -- ^ Witness to convert.
        -> ConvertM a (Witness S.Name)

convertWitnessX kenv ww
 = let down = convertWitnessX kenv
   in  case ww of
            WVar n      -> liftM  WVar  (convertU n)
            WCon wc     -> liftM  WCon  (convertWiConX kenv wc)
            WApp w1 w2  -> liftM2 WApp  (down w1) (down w2)
            WJoin w1 w2 -> liftM2 WApp  (down w1) (down w2)
            WType t     -> liftM  WType (convertT kenv t)


convertWiConX
        :: Show a
        => Env L.Name                   -- ^ Kind environment. 
        -> WiCon L.Name                 -- ^ Witness constructor to convert.
        -> ConvertM a (WiCon S.Name)    
convertWiConX kenv wicon            
 = case wicon of
        WiConBuiltin w
         -> return $ WiConBuiltin w

        WiConBound n t 
         -> liftM2 WiConBound
                        (convertU n)
                        (convertT kenv t)


-------------------------------------------------------------------------------
convertCtorAppX 
        :: Show a
        => Platform             -- ^ Platform specification.
        -> DataDefs L.Name      -- ^ Data type definitions.
        -> Env L.Name           -- ^ Kind environment.
        -> Env L.Name           -- ^ Type environment.
        -> AnTEC a L.Name       -- ^ Annotation from deconstructed application node.
        -> DaCon L.Name         -- ^ Data constructor being applied.
        -> [Exp (AnTEC a L.Name) L.Name]
        -> ConvertM a (Exp a S.Name)

convertCtorAppX pp defs kenv tenv (AnTEC _ _ _ a) dc xsArgs

        -- Pass through unboxed literals.
        | Just (L.NameBool b)   <- takeNameOfDaCon dc
        , []                    <- xsArgs
        = return $ S.xBool a b

        | Just (L.NameNat i)    <- takeNameOfDaCon dc
        , []                    <- xsArgs
        = return $ S.xNat  a i

        | Just (L.NameInt i)    <- takeNameOfDaCon dc
        , []                    <- xsArgs
        = return $ S.xInt  a i

        | Just (L.NameWord i bits) <- takeNameOfDaCon dc
        , []                       <- xsArgs
        = return $ S.xWord a i bits

        -- Construct algbraic data that has a finite number of data constructors.
        | Just nCtor            <- takeNameOfDaCon dc
        , Just ctorDef          <- Map.lookup nCtor $ dataDefsCtors defs
        , Just dataDef          <- Map.lookup (dataCtorTypeName ctorDef) $ dataDefsTypes defs
        = do    
                -- Get the prime region variable that holds the outermost constructor.
                --   For types like Unit, there is no prime region, so put them in the 
                --   top-level region of the program.
                rPrime
                 <- case xsArgs of
                        [] 
                         -> return S.rTop

                        XType (TVar u) : _
                         | Just tu      <- Env.lookup u kenv
                         -> if isRegionKind tu
                             then do u'      <- convertU u
                                     return  $ TVar u'
                             else return S.rTop

                        _ -> error $ "convertCtorAppX: prime region var not in environment" 


                -- Convert the types of each field.
                let makeFieldType x
                        = case takeAnnotOfExp x of
                                Nothing  -> return Nothing
                                Just a'  -> liftM Just $ convertT kenv (annotType a')

                xsArgs'         <- mapM (convertAtomX pp defs kenv tenv) xsArgs
                tsArgs'         <- mapM makeFieldType xsArgs
                constructData pp kenv tenv a
                                dataDef ctorDef
                                rPrime xsArgs' tsArgs'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorAppX _ _ _ _ _ _nCtor _xsArgs
        = throw $ ErrorMalformed "convertCtorAppX: invalid constructor application"


-- Alt ------------------------------------------------------------------------
convertAlt 
        :: Show a
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type declarations.
        -> Env L.Name                   -- ^ Kind environment.
        -> Env L.Name                   -- ^ Type environment.
        -> a                            -- ^ Annotation from case expression.
        -> Bound L.Name                 -- ^ Bound of scrutinee.
        -> Type  L.Name                 -- ^ Type  of scrutinee
        -> Alt (AnTEC a L.Name) L.Name  -- ^ Alternative to convert.
        -> ConvertM a (Alt a S.Name)

convertAlt pp defs kenv tenv a uScrut tScrut alt
 = case alt of
        AAlt PDefault x
         -> do  x'      <- convertBodyX pp defs kenv tenv x
                return  $ AAlt PDefault x'

        -- Match against literal unboxed values.
        AAlt (PData uCtor []) x
         | UPrim nCtor _        <- uCtor
         , case nCtor of
                L.NameInt{}     -> True
                L.NameWord{}    -> True
                L.NameBool{}    -> True
                _               -> False
         -> do  uCtor'  <- convertU uCtor
                xBody1  <- convertBodyX pp defs kenv tenv x
                return  $ AAlt (PData uCtor' []) xBody1

        -- Match against algebraic data with a finite number
        -- of data constructors.
        AAlt (PData uCtor bsFields) x
         | Just nCtor    <- case uCtor of
                                UName n   -> Just n
                                UPrim n _ -> Just n
                                _         -> Nothing
         , Just ctorDef   <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                let tenv'       = Env.extends bsFields tenv 
                uScrut'         <- convertU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let uTag        = UPrim (S.NameTag iTag) S.tTag

                -- Get the address of the payload.
                bsFields'       <- mapM (convertB kenv) bsFields

                -- Convert the right of the alternative.
                xBody1          <- convertBodyX pp defs kenv tenv' x

                -- Add let bindings to unpack the constructor.
                tScrut'         <- convertT kenv tScrut
                let Just trPrime = takePrimeRegion tScrut'
                xBody2           <- destructData pp a uScrut' ctorDef trPrime bsFields' xBody1

                return  $ AAlt (PData uTag []) xBody2

        AAlt{}          
         -> throw ErrorInvalidAlt


-- Data Constructor -----------------------------------------------------------
convertCtor 
        :: Show a
        => Platform             -- ^ Platform specification.
        -> DataDefs L.Name      -- ^ Data type definitions.
        -> Env L.Name           -- ^ Kind environment.
        -> Env L.Name           -- ^ Type environment.
        -> a                    -- ^ Annotation to attach to exp nodes.
        -> DaCon L.Name         -- ^ Data constructor to convert.
        -> ConvertM a (Exp a S.Name)

convertCtor pp defs kenv tenv a dc
 | Just n      <- takeNameOfDaCon dc
 = case n of
        -- Literal values.
        L.NameBool v            -> return $ S.xBool a v
        L.NameNat  i            -> return $ S.xNat  a i
        L.NameInt  i            -> return $ S.xInt  a i
        L.NameWord i bits       -> return $ S.xWord a i bits

        -- A Zero-arity data constructor.
        nCtor
         | Just ctorDef         <- Map.lookup nCtor $ dataDefsCtors defs
         , Just dataDef         <- Map.lookup (dataCtorTypeName ctorDef) $ dataDefsTypes defs
         -> do  -- Put zero-arity data constructors in the top-level region.
                let rPrime      = S.rTop
                constructData pp kenv tenv a dataDef ctorDef rPrime [] []

        _ -> throw $ ErrorMalformed "convertCtor: invalid constructor"

 | otherwise
 = throw $ ErrorMalformed "convertCtor: invalid constructor"
