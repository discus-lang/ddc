
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
import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Check.Monad              (throw, result)
import DDC.Core.Check                    (AnTEC(..))
import DDC.Type.Env                      (KindEnv, TypeEnv)
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
        -> KindEnv L.Name                       -- ^ Kind environment.
        -> TypeEnv L.Name                       -- ^ Type environment.
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
        -> KindEnv L.Name
        -> TypeEnv L.Name
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
        x1              <- convertExpX ExpTop pp defs kenv tenv' $ moduleBody mm

        -- Converting the body will also expand out code to construct,
        -- the place-holder '()' inside the top-level lets.
        -- We don't want that, so just replace that code with a fresh unit.
        let Just a      = takeAnnotOfExp x1
        let (lts', _)   = splitXLets x1
        let x2          = makeXLets a lts' (xUnit a)

        -- Build the output module.
        let mm_salt 
                = ModuleCore
                { moduleName           = moduleName mm
                , moduleExportKinds    = Map.empty
                , moduleExportTypes    = Map.empty
                , moduleImportKinds    = S.runtimeImportKinds
                , moduleImportTypes    = Map.union S.runtimeImportTypes tsImports'
                , moduleBody           = x2 }

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
-- | The context we're converting the expression in.
--     We keep track of this during conversion to ensure we don't produce
--     code outside the Salt language fragment. For example, in Salt we can only
--     have value variables, types and witnesses as function arguments, not general
--     expressions.
data ExpContext
        = ExpTop        -- ^ At the top-level of the module.
        | ExpFun        -- ^ At the top-level of a function.
        | ExpBody       -- ^ In the body of a function.
        | ExpBind       -- ^ In the right of a let-binding.
        | ExpArg        -- ^ In a function argument.
        deriving (Show, Eq, Ord)


-- | Convert the body of a supercombinator to Salt.
convertExpX 
        :: Show a 
        => ExpContext                   -- ^ What context we're converting in.
        -> Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> KindEnv L.Name               -- ^ Kind environment.
        -> TypeEnv L.Name               -- ^ Type environment.
        -> Exp (AnTEC a L.Name) L.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a S.Name)

convertExpX ctx pp defs kenv tenv xx
 = let downArgX     = convertExpX     ExpArg pp defs kenv tenv
       downCtorAppX = convertCtorAppX pp defs kenv tenv
   in case xx of

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


        -- Type abstractions can only appear at the top-level of a function.
        --   Keep region and data type lambdas, but ditch the others.
        XLAM a b x
         | ExpFun       <- ctx
         ,   (isRegionKind $ typeOfBind b)
          || (isDataKind   $ typeOfBind b)
         -> do  let a'    =  annotTail a
                b'        <- convertB kenv b

                let kenv' =  Env.extend b kenv
                x'        <- convertExpX ctx pp defs kenv' tenv x

                return $ XLAM a' b' x'

         | ExpFun       <- ctx
         -> do  let kenv'       = Env.extend b kenv
                convertExpX ctx pp defs kenv' tenv x

         | otherwise
         -> error $ "convertBodyX: can't convert XLAM in " ++ show ctx


        -- Value abstractions can only appear at the top-level of a fucntion.
        XLam a b x
         | ExpFun       <- ctx
         -> let tenv'   = Env.extend b tenv
            in case universeFromType1 kenv (typeOfBind b) of
                Just UniverseData    
                 -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertB kenv b) 
                        (convertExpX ctx pp defs kenv tenv' x)

                Just UniverseWitness 
                 -> liftM3 XLam
                        (return $ annotTail a)
                        (convertB kenv b)
                        (convertExpX ctx pp defs kenv tenv' x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b
         | otherwise
         -> error $ "convertBodyX: can't convert XLam in " ++ show ctx


        -- Data constructor applications.
        XApp a xa xb
         | (x1, xsArgs)           <- takeXApps' xa xb
         , XCon _ dc <- x1
         -> downCtorAppX a dc xsArgs

        -- Primitive operations.
        XApp a xa xb
         | (x1, xsArgs)          <- takeXApps' xa xb
         , XVar _ UPrim{}        <- x1
         -> do  x1'     <- downArgX x1
                xsArgs' <- mapM downArgX xsArgs

                return $ makeXApps (annotTail a) x1' xsArgs'

        -- Function application.
        -- TODO: This only works for full application. 
        --       At least check for the other cases.
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, xsArgs) <- takeXApps' xa xb
         -> do  x1'     <- downArgX x1
                xsArgs' <- mapM downArgX xsArgs
                return  $ makeXApps a' x1' xsArgs'


        -- let-expressions.
        XLet a lts x2
         | ctx <= ExpBind
         -> do  -- Convert the bindings.
                lts'            <- convertLetsX pp defs kenv tenv lts

                -- Convert the body of the expression.
                let (bs1, bs0)  = bindsOfLets lts
                let kenv'       = Env.extends bs1 kenv
                let tenv'       = Env.extends bs0 tenv
                x2'             <- convertExpX ExpBody pp defs kenv' tenv' x2

                return $ XLet (annotTail a) lts' x2'

        XLet{}
         -> throw $ ErrorNotNormalized ("let-expression")


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
         -> do  xScrut' <- convertExpX ExpArg pp defs kenv tenv xScrut
                alts'   <- mapM (convertAlt (min ctx ExpBody) pp defs kenv tenv a' uScrut tScrut) 
                                alts
                return  $  XCase a' xScrut' alts'

        -- Match against finite algebraic data.
        --   The branch is against the constructor tag.
        XCase (AnTEC tX _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon (TyConBound (UPrim nType _) _) : _ <- takeTApps tScrut
         , L.NameDataTyCon _                       <- nType
         -> do  x'      <- convertExpX ExpArg pp defs kenv tenv xScrut
                tX'     <- convertT kenv tX
                alts'   <- mapM (convertAlt (min ctx ExpBody) pp defs kenv tenv a' uScrut tScrut) 
                                alts

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

        XCase{} 
         -> throw $ ErrorNotNormalized ("case expression")


        -- Casts.
        XCast _ _ x
         -> convertExpX (min ctx ExpBody) pp defs kenv tenv x


        -- Types can only appear as the arguments in function applications.
        XType t
         | ExpArg <- ctx  -> liftM XType (convertT kenv t)
         | otherwise      -> throw $ ErrorNotNormalized ("type expresison")


        -- Witnesses can only appear as the arguments to function applications.
        XWitness w      
         | ExpArg <- ctx  -> liftM XWitness (convertWitnessX kenv w)
         | otherwise      -> throw $ ErrorNotNormalized ("witness expression")



-------------------------------------------------------------------------------
-- | Convert a let-binding to Salt.
convertLetsX 
        :: Show a 
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> KindEnv L.Name               -- ^ Kind environment.
        -> TypeEnv L.Name               -- ^ Type environment.
        -> Lets (AnTEC a L.Name) L.Name -- ^ Expression to convert.
        -> ConvertM a (Lets a S.Name)

convertLetsX pp defs kenv tenv lts
 = case lts of
        LRec bxs
         -> do  let tenv'       = Env.extends (map fst bxs) tenv
                let (bs, xs)    = unzip bxs
                bs'             <- mapM (convertB kenv) bs
                xs'             <- mapM (convertExpX ExpFun pp defs kenv tenv') xs
                return  $ LRec $ zip bs' xs'

        LLet LetStrict b x1
         -> do  let tenv'       = Env.extend b tenv
                b'              <- convertB       kenv b
                x1'             <- convertExpX ExpBind pp defs kenv tenv' x1
                return  $ LLet LetStrict b' x1'

        LLet LetLazy{} _ _
         ->     error "DDC.Core.Lite.Convert.toSaltX: XLet lazy not handled yet"

        LLetRegions b bs
         -> do  b'              <- mapM (convertB kenv) b
                let kenv'       = Env.extends b kenv
                bs'             <- mapM (convertB kenv') bs
                return  $ LLetRegions b' bs'
  
        LWithRegion{}
         ->     throw $ ErrorMalformed "LWithRegion should not appear in Lite code."


-------------------------------------------------------------------------------
-- | Convert a witness expression to Salt
convertWitnessX
        :: Show a
        => KindEnv L.Name               -- ^ Kind enviornment
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
        => KindEnv L.Name               -- ^ Kind environment. 
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
        => Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type definitions.
        -> KindEnv L.Name               -- ^ Kind environment.
        -> TypeEnv L.Name               -- ^ Type environment.
        -> AnTEC a L.Name               -- ^ Annotation from deconstructed application node.
        -> DaCon L.Name                 -- ^ Data constructor being applied.
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

        -- Handle the unit constructor.
        -- TODO: use a shared object instead of allocating one.
        | DaConUnit      <- daConName dc
        = do    return  $ S.xAllocBoxed a S.rTop 0 (S.xNat a 0)

        -- Construct algbraic data that has a finite number of data constructors.
        | Just nCtor     <- takeNameOfDaCon dc
        , Just ctorDef   <- Map.lookup nCtor $ dataDefsCtors defs
        , Just dataDef   <- Map.lookup (dataCtorTypeName ctorDef) $ dataDefsTypes defs
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

                xsArgs'         <- mapM (convertExpX ExpArg pp defs kenv tenv) xsArgs
                tsArgs'         <- mapM makeFieldType xsArgs
                constructData pp kenv tenv a
                                dataDef ctorDef
                                rPrime xsArgs' tsArgs'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorAppX _ _ _ _ _ _nCtor _xsArgs
        = throw $ ErrorMalformed "convertCtorAppX: invalid constructor application"


-- Alt ------------------------------------------------------------------------
-- | Convert a Lite alternative to Salt.
convertAlt 
        :: Show a
        => ExpContext
        -> Platform                     -- ^ Platform specification.
        -> DataDefs L.Name              -- ^ Data type declarations.
        -> KindEnv L.Name               -- ^ Kind environment.
        -> TypeEnv L.Name               -- ^ Type environment.
        -> a                            -- ^ Annotation from case expression.
        -> Bound L.Name                 -- ^ Bound of scrutinee.
        -> Type  L.Name                 -- ^ Type  of scrutinee
        -> Alt (AnTEC a L.Name) L.Name  -- ^ Alternative to convert.
        -> ConvertM a (Alt a S.Name)

convertAlt ctx pp defs kenv tenv a uScrut tScrut alt
 = case alt of
        AAlt PDefault x
         -> do  x'      <- convertExpX ctx pp defs kenv tenv x
                return  $ AAlt PDefault x'

        -- Match against literal unboxed values.
        AAlt (PData dc []) x
         | Just nCtor           <- takeNameOfDaCon dc
         , case nCtor of
                L.NameInt{}     -> True
                L.NameWord{}    -> True
                L.NameBool{}    -> True
                _               -> False

         -> do  dc'     <- convertDC kenv dc
                xBody1  <- convertExpX ctx pp defs kenv tenv x
                return  $ AAlt (PData dc' []) xBody1

        -- TODO: Handle matching unit constructors.

        -- Match against algebraic data with a finite number
        -- of data constructors.
        AAlt (PData dc bsFields) x
         | Just nCtor   <- takeNameOfDaCon dc
         , Just ctorDef <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                let tenv'       = Env.extends bsFields tenv 
                uScrut'         <- convertU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let dcTag       = mkDaConAlg (S.NameTag iTag) S.tTag

                -- Get the address of the payload.
                bsFields'       <- mapM (convertB kenv) bsFields

                -- Convert the right of the alternative.
                xBody1          <- convertExpX ctx pp defs kenv tenv' x

                -- Add let bindings to unpack the constructor.
                tScrut'         <- convertT kenv tScrut
                let Just trPrime = takePrimeRegion tScrut'
                xBody2           <- destructData pp a uScrut' ctorDef trPrime bsFields' xBody1
                return  $ AAlt (PData dcTag []) xBody2

        AAlt{}          
         -> throw ErrorInvalidAlt


-- Data Constructor -----------------------------------------------------------
-- | Expand out code to build a data constructor.
convertCtor 
        :: Show a
        => Platform             -- ^ Platform specification.
        -> DataDefs L.Name      -- ^ Data type definitions.
        -> KindEnv L.Name       -- ^ Kind environment.
        -> TypeEnv L.Name       -- ^ Type environment.
        -> a                    -- ^ Annotation to attach to exp nodes.
        -> DaCon L.Name         -- ^ Data constructor to convert.
        -> ConvertM a (Exp a S.Name)

convertCtor pp defs kenv tenv a dc

 -- TODO: Use a shared unit constructor instead of allocating a new one.
 | DaConUnit    <- daConName dc
 =      return $ S.xAllocBoxed a S.rTop 0 (S.xNat a 0)

 | Just n       <- takeNameOfDaCon dc
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
