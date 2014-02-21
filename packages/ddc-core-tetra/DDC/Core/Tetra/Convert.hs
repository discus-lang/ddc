-- | Conversion of Disciple Lite to Disciple Salt.
--
module DDC.Core.Tetra.Convert
        ( saltOfTetraModule
        , Error(..))
where
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Data
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Base
import DDC.Core.Salt.Convert.Init
import DDC.Core.Salt.Platform
import DDC.Core.Transform.LiftX
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A

import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Env                      (KindEnv, TypeEnv)
import qualified DDC.Type.Env            as Env

import Data.Set                          (Set)
import DDC.Control.Monad.Check           (throw, evalCheck)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import Control.Monad
import Data.Maybe

import DDC.Base.Pretty


---------------------------------------------------------------------------------------------------
-- | Convert a Core Tetra module to Core Salt.
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      have type annotations on every bound variable and constructor,
--      be a-normalised,
--      have saturated function applications,
--      not have over-applied function applications.
--      If not then `Error`.
--
--   The output code contains:
--      debruijn indices.
--       These then need to be eliminated before it will pass the Salt fragment
--       checks.
--
saltOfTetraModule
        :: Show a
        => Platform                             -- ^ Platform specification.
        -> A.Config                             -- ^ Runtime configuration.
        -> DataDefs E.Name                      -- ^ Data type definitions.
        -> KindEnv  E.Name                      -- ^ Kind environment.
        -> TypeEnv  E.Name                      -- ^ Type environment.
        -> Module (AnTEC a E.Name) E.Name       -- ^ Lite module to convert.
        -> Either (Error a) (Module a A.Name)   -- ^ Salt module.

saltOfTetraModule platform runConfig defs kenv tenv mm
 = {-# SCC saltOfTetraModule #-}
   evalCheck () $ convertM platform runConfig defs kenv tenv mm


---------------------------------------------------------------------------------------------------
convertM 
        :: Show a
        => Platform
        -> A.Config
        -> DataDefs E.Name
        -> KindEnv  E.Name
        -> TypeEnv  E.Name
        -> Module (AnTEC a E.Name) E.Name 
        -> ConvertM a (Module a A.Name)

convertM pp runConfig defs kenv tenv mm
  = do  
        -- Convert signatures of exported functions.
        tsExports' <- mapM convertExportM $ moduleExportValues mm

        -- Convert signatures of imported functions.
        tsImports' <- mapM convertImportM $ moduleImportValues mm

        -- Convert the body of the module to Salt.
        let ntsImports  
                   = [BName n (typeOfImportSource src) 
                        | (n, src) <- moduleImportValues mm]
        let tenv'  = Env.extends ntsImports tenv
        
        let defs'  = unionDataDefs defs
                   $ fromListDataDefs (moduleDataDefsLocal mm)

        let penv   = TopEnv
                   { topEnvPlatform     = pp
                   , topEnvDataDefs     = defs'
                   , topEnvSupers       = moduleTopBinds mm 
                   , topEnvImportValues = Set.fromList $ map fst $ moduleImportValues mm }

        x1         <- convertExpX penv kenv tenv' ExpTop
                   $  moduleBody mm

        -- Converting the body will also expand out code to construct,
        -- the place-holder '()' inside the top-level lets.
        -- We don't want that, so just replace that code with a fresh unit.
        let a           = annotOfExp x1
        let (lts', _)   = splitXLets x1
        let x2          = xLets a lts' (xUnit a)

        -- Build the output module.
        let mm_salt 
                = ModuleCore
                { moduleName           = moduleName mm

                  -- None of the types imported by Lite modules are relevant
                  -- to the Salt language.
                , moduleExportTypes    = []
                , moduleExportValues   = tsExports'

                , moduleImportTypes    = Map.toList $ A.runtimeImportKinds
                , moduleImportValues   = (Map.toList A.runtimeImportTypes) ++ tsImports'

                  -- Data constructors and pattern matches should have been
                  -- flattenedinto primops, so we don't need the data type
                  -- definitions.
                , moduleDataDefsLocal  = []

                , moduleBody           = x2 }

        -- If this is the 'Main' module then add code to initialise the 
        -- runtime system. This will fail if given a Main module with no
        -- 'main' function.
        mm_init <- case initRuntime runConfig mm_salt of
                        Nothing   -> throw ErrorMainHasNoMain
                        Just mm'  -> return mm'

        return $ mm_init


---------------------------------------------------------------------------------------------------
-- | Convert an export spec.
convertExportM
        :: (E.Name, ExportSource E.Name)                
        -> ConvertM a (A.Name, ExportSource A.Name)

convertExportM (n, esrc)
 = do   n'      <- convertBindNameM n
        esrc'   <- convertExportSourceM esrc
        return  (n', esrc')


-- Convert an export source.
convertExportSourceM 
        :: ExportSource E.Name
        -> ConvertM a (ExportSource A.Name)

convertExportSourceM esrc
 = case esrc of
        ExportSourceLocal n t
         -> do  n'      <- convertBindNameM n
                t'      <- convertRepableT Env.empty t
                return  $ ExportSourceLocal n' t'

        ExportSourceLocalNoType n
         -> do  n'      <- convertBindNameM n
                return  $ ExportSourceLocalNoType n'


---------------------------------------------------------------------------------------------------
-- | Convert an import spec.
convertImportM
        :: (E.Name, ImportSource E.Name)
        -> ConvertM a (A.Name, ImportSource A.Name)

convertImportM (n, isrc)
 = do   n'      <- convertImportNameM n
        isrc'   <- convertImportSourceM isrc
        return  (n', isrc')


-- | Convert an imported name.
--   These can be variable names for values, 
--   or variable or constructor names for type imports.
convertImportNameM :: E.Name -> ConvertM a A.Name
convertImportNameM n
 = case n of
        E.NameVar str   -> return $ A.NameVar str
        E.NameCon str   -> return $ A.NameCon str
        _               -> throw  $ ErrorInvalidBinder n


-- | Convert an import source.
convertImportSourceM 
        :: ImportSource E.Name
        -> ConvertM a (ImportSource A.Name)

convertImportSourceM isrc
 = case isrc of
        ImportSourceAbstract t
         -> do  t'      <- convertRepableT Env.empty t
                return $ ImportSourceAbstract t'

        ImportSourceModule mn n t
         -> do  n'      <- convertBindNameM n
                t'      <- convertRepableT Env.empty t
                return  $ ImportSourceModule mn n' t'

        ImportSourceSea str t
         -> do  t'      <- convertRepableT Env.empty t 
                return  $ ImportSourceSea str t'


---------------------------------------------------------------------------------------------------
-- | Information about the top-level environment.
data TopEnv
        = TopEnv
        { -- Platform we're converting to.
          topEnvPlatform        :: Platform

          -- Data type definitions.
        , topEnvDataDefs        :: DataDefs E.Name

          -- Names of top-level supercombinators that are directly callable.
        , topEnvSupers          :: Set E.Name 

          -- Names of imported values that can be refered to directly.
        , topEnvImportValues    :: Set E.Name }


-- | The context we're converting the expression in.
--     We keep track of this during conversion to ensure we don't produce
--     code outside the Salt language fragment. For example, in Salt a function
--     can only be applied to a value variable, type or witness -- and not
--     a general expression.
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
        => TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> ExpContext                   -- ^ What context we're converting in.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertExpX penv kenv tenv ctx xx
 = let pp           = topEnvPlatform  penv
       defs         = topEnvDataDefs  penv
       downArgX     = convertExpX     penv kenv tenv ExpArg
       downPrimArgX = convertPrimArgX penv kenv tenv ExpArg
       downCtorAppX = convertCtorAppX penv kenv tenv

   in case xx of

        ---------------------------------------------------
        XVar _ UIx{}
         -> throw $ ErrorMalformed 
                  $ "Cannot convert program with anonymous value binders."

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertValueU u
                return  $  XVar a' u'

        XCon a dc
         -> do  xx'     <- convertCtorAppX penv kenv tenv a dc []
                return  xx'

        ---------------------------------------------------
        -- Type lambdas can only appear at the top-level of a function.
        --   We keep region lambdas but ditch the others. Polymorphic values
        --   are represented in generic boxed form, so we never need to 
        --   build a type abstraction of some other kind.
        XLAM a b x
         | ExpFun       <- ctx
         , isRegionKind $ typeOfBind b
         -> do  let a'    =  annotTail a
                b'        <- convertTypeB b

                let kenv' =  Env.extend b kenv
                x'        <- convertExpX penv kenv' tenv ctx x

                return $ XLAM a' b' x'

         -- When a function is fully polymorphic in some boxed data type,
         -- then the type lambda Tetra is converted to a region lambda in Salt
         -- which binds the region the object is in.
         | ExpFun       <- ctx
         , isDataKind $ typeOfBind b
         , BName (E.NameVar str) k <- b
         , isDataKind k
         , str'         <- str ++ "$r"
         , b'           <- BName (A.NameVar str') kRegion
         -> do  let a'  = annotTail a
                
                let kenv' = Env.extend b kenv
                x'      <- convertExpX penv kenv' tenv ctx x

                return $ XLAM a' b' x'

         -- Erase effect lambdas.
         | ExpFun       <- ctx
         , isEffectKind $ typeOfBind b
         -> do  let kenv'       = Env.extend b kenv
                convertExpX penv kenv' tenv ctx x

         -- A type abstraction that we can't convert to Salt.
         | otherwise
         -> throw $ ErrorMalformed
                  $ "Cannot convert XLAM in this context " ++ show ctx


        ---------------------------------------------------
        -- Function abstractions can only appear at the top-level of a fucntion.
        XLam a b x
         | ExpFun       <- ctx
         -> let tenv'   = Env.extend b tenv
            in case universeFromType1 kenv (typeOfBind b) of
                Just UniverseData    
                 -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertRepableB kenv b) 
                        (convertExpX penv kenv tenv' ctx x)

                Just UniverseWitness 
                 -> liftM3 XLam
                        (return $ annotTail a)
                        (convertRepableB kenv b)
                        (convertExpX penv kenv tenv' ctx x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b
         | otherwise
         -> throw $ ErrorMalformed 
                  $ "Cannot convert XLam in this context " ++ show ctx


        ---------------------------------------------------
        -- Wrapping of pure values into boxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBIx, XType _ tBx, XCon _ c]) <- takeXPrimApps xx
         , isBoxableIndexType tBIx
         , isBoxedRepType     tBx
         , Just dt      <- makeDataTypeForBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> do  
                let a'  = annotTail a
                xArg'   <- convertLitCtorX a' c
                tBIx'   <- convertIndexT tBIx

                constructData pp kenv tenv a'
                        dt dc A.rTop [xArg'] [tBIx']


        ---------------------------------------------------
        -- Unwrapping of boxed values into pure values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBx, XType _ tBIx, xArg])    <- takeXPrimApps xx
         , isBoxedRepType     tBx
         , isBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> do  
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT tBIx
                tBx'    <- convertRepableT kenv tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop 
                                [BAnon tBIx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg'))
                                  x'

        ---------------------------------------------------
        -- Boxing of unboxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for user-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tUx, XType _ tBx, xArg])      <- takeXPrimApps xx
         , isUnboxedRepType tUx
         , isBoxedRepType   tBx
         , Just tBIx    <- takeIndexOfBoxedRepType tBx
         , Just dt      <- makeDataTypeForBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> do  
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT tBIx

                constructData pp kenv tenv a'
                        dt dc A.rTop [xArg'] [tBIx']


        ---------------------------------------------------
        -- Unboxing of boxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBx, XType _ tUx, xArg])     <- takeXPrimApps xx
         , isBoxedRepType   tBx
         , isUnboxedRepType tUx
         , Just tBIx    <- takeIndexOfBoxedRepType tBx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> do
                let a'  = annotTail a
                xArg'   <- downArgX xArg
                tBIx'   <- convertIndexT   tBIx
                tBx'    <- convertRepableT kenv tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop 
                                [BAnon tBIx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg'))
                                  x'

        
        ---------------------------------------------------
        -- Saturated application of a primitive data constructor,
        --   including the Unit data constructor.
        --   The types of these are directly attached.
        XApp a xa xb
         | (x1, xsArgs)            <- takeXApps1 xa xb
         , XCon _ dc               <- x1
         , Just tCon               <- takeTypeOfDaCon dc
         , length xsArgs == arityOfType tCon
         -> downCtorAppX a dc xsArgs

        -- Fully applied user-defined data constructor application.
        --   The types of these are in the defs list.
        XApp a xa xb
         | (x1, xsArgs   )          <- takeXApps1 xa xb
         , XCon _ dc@(DaConBound n) <- x1
         , Just dataCtor            <- Map.lookup n (dataDefsCtors defs)
         , length xsArgs 
                == length (dataCtorTypeParams dataCtor)
                +  length (dataCtorFieldTypes dataCtor)

         -> downCtorAppX a dc xsArgs


        ---------------------------------------------------
        -- Saturated application of a primitive operator.
        XApp a xa xb
         | (x1, xsArgs)               <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim tPrim) <- x1

         -- The primop is saturated.
         , length xsArgs == arityOfType tPrim

         -- All the value arguments have representatable types.
         , all isSomeRepType
                $  map (annotType . annotOfExp)
                $  filter (not . isXType) xsArgs

         -- The result is representable.
         , isSomeRepType (annotType a)

         -> do  x1'     <- downArgX x1
                xsArgs' <- mapM downPrimArgX xsArgs
                
                case nPrim of
                 -- The Tetra type of these is also parameterised by the type of the
                 -- boolean result, so that we can choose between value type and unboxed
                 -- versions. In the Salt version we only need the first type parameter.
                 E.NamePrimArith o
                  |  elem o [ E.PrimArithEq, E.PrimArithNeq
                            , E.PrimArithGt, E.PrimArithLt
                            , E.PrimArithLe, E.PrimArithGe ]
                  ,  [t1, _t2, z1, z2] <- xsArgs'
                  -> return $ xApps (annotTail a) x1' [t1, z1, z2]

                 _ -> return $ xApps (annotTail a) x1' xsArgs'


        ---------------------------------------------------
        -- Saturated application of a top-level supercombinator or imported function.
        --  This does not cover application of primops, the above case should
        --  fire for these.
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, xsArgs) <- takeXApps1 xa xb
         
         -- The thing being applied is a named function that is defined
         -- at top-level, or imported directly.
         , XVar _ (UName n) <- x1
         ,   Set.member n (topEnvSupers       penv)
          || Set.member n (topEnvImportValues penv)

         -- The function is saturated.
         , length xsArgs == arityOfType (annotType $ annotOfExp x1)

         -> do  -- Convert the functional part.
                x1'     <- downArgX x1

                -- Convert the arguments.
                -- Effect type and witness arguments are discarded here.
                xsArgs' <- liftM catMaybes 
                        $  mapM (convertOrDiscardSuperArgX penv kenv tenv) xsArgs
                        
                return  $ xApps a' x1' xsArgs'

        
        ---------------------------------------------------
        -- let-expressions.
        XLet a lts x2
         | ctx <= ExpBind
         -> do  -- Convert the bindings.
                lts'            <- convertLetsX penv kenv tenv lts

                -- Convert the body of the expression.
                let (bs1, bs0)  = bindsOfLets lts
                let kenv'       = Env.extends bs1 kenv
                let tenv'       = Env.extends bs0 tenv
                x2'             <- convertExpX penv kenv' tenv' ExpBody x2

                return $ XLet (annotTail a) lts' x2'

        XLet{}
         -> throw $ ErrorNotNormalized "Unexpected let-expression."


        ---------------------------------------------------
        -- Match against literal unboxed values.
        --  The branch is against the literal value itself.
        XCase (AnTEC _ _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon (TyConBound (UPrim nType _) _)  <- tScrut
         , E.NamePrimTyCon _                    <- nType
         -> do  
                -- Convert the scrutinee.
                xScrut' <- convertExpX penv kenv tenv ExpArg xScrut

                -- Convert the alternatives.
                alts'   <- mapM (convertAlt penv kenv tenv (min ctx ExpBody)
                                        a' uScrut tScrut) 
                                alts

                return  $  XCase a' xScrut' alts'


        ---------------------------------------------------
        -- Match against finite algebraic data.
        --   The branch is against the constructor tag.
        XCase (AnTEC tX _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon _ : _   <- takeTApps tScrut
         , isSomeRepType tScrut
         -> do  
                -- Convert scrutinee, and determine its prime region.
                x'      <- convertExpX     penv kenv tenv ExpArg xScrut
                tX'     <- convertRepableT kenv tX

                tScrut' <- convertRepableT kenv tScrut
                let tPrime = fromMaybe A.rTop
                           $ takePrimeRegion tScrut'

                -- Convert alternatives.
                alts'   <- mapM (convertAlt penv kenv tenv (min ctx ExpBody)
                                        a' uScrut tScrut) 
                                alts

                -- If the Tetra program does not have a default alternative
                -- then add our own to the Salt program. We need this to handle
                -- the case where the Tetra program does not cover all the 
                -- possible cases.
                let hasDefaultAlt
                        = any isPDefault [p | AAlt p _ <- alts]

                let newDefaultAlt
                        | hasDefaultAlt = []
                        | otherwise     = [AAlt PDefault (A.xFail a' tX')]

                return  $ XCase a' (A.xGetTag a' tPrime x') 
                        $ alts' ++ newDefaultAlt


        ---------------------------------------------------
        -- Trying to matching against something that isn't a primitive numeric
        -- type or alebraic data.
        -- 
        -- We don't handle matching purely polymorphic data against the default
        -- alterative,  (\x. case x of { _ -> x}), because the type of the
        -- scrutinee isn't constrained to be an algebraic data type. These dummy
        -- expressions need to be eliminated before conversion.
        XCase{} 
         -> throw $ ErrorNotNormalized "Invalid case expression."


        ---------------------------------------------------
        -- Casts.
        XCast _ _ x
         -> convertExpX penv kenv tenv (min ctx ExpBody) x


        -- We shouldn't find any naked types.
        -- These are handled above in the XApp case.
        XType{}
          -> throw $ ErrorNotNormalized "Unexpected type argument."


        -- We shouldn't find any naked witnesses.
        XWitness{}
          -> throw $ ErrorNotNormalized "Unexpected witness expression."

        -- Expression can't be converted.
        _ -> throw $ ErrorNotNormalized 
                   $ "Cannot convert expression.\n"
                   ++ (renderIndent $ ppr xx)


---------------------------------------------------------------------------------------------------
-- | Convert a let-binding to Salt.
convertLetsX 
        :: Show a 
        => TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Lets (AnTEC a E.Name) E.Name -- ^ Expression to convert.
        -> ConvertM a (Lets a A.Name)

convertLetsX penv kenv tenv lts
 = case lts of
        LRec bxs
         -> do  let tenv'    = Env.extends (map fst bxs) tenv
                let (bs, xs) = unzip bxs
                bs'          <- mapM (convertValueB kenv) bs
                xs'          <- mapM (convertExpX penv kenv tenv' ExpFun) xs
                return  $ LRec $ zip bs' xs'

        LLet b x1
         -> do  let tenv'    = Env.extend b tenv
                b'           <- convertValueB kenv b
                x1'          <- convertExpX   penv kenv tenv' ExpBind x1
                return  $ LLet b' x1'

        -- TODO: convert witness bindings
        LPrivate b mt _bs
         -> do  b'           <- mapM convertTypeB b
--              let kenv'    = Env.extends b kenv
--              bs'          <- mapM (convertTypeB kenv') bs    
                mt'          <- case mt of
                                 Nothing -> return Nothing
                                 Just t  -> liftM Just $ convertRegionT kenv t
                return  $ LPrivate b' mt' [] 
  
        LWithRegion{}
         ->     throw $ ErrorMalformed "Cannot convert LWithRegion construct."


---------------------------------------------------------------------------------------------------
-- | Convert a Lite alternative to Salt.
convertAlt 
        :: Show a
        => TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> ExpContext                   -- ^ Context of enclosing case-expression.
        -> a                            -- ^ Annotation from case expression.
        -> Bound E.Name                 -- ^ Bound of scrutinee.
        -> Type  E.Name                 -- ^ Type  of scrutinee
        -> Alt (AnTEC a E.Name) E.Name  -- ^ Alternative to convert.
        -> ConvertM a (Alt a A.Name)

convertAlt penv kenv tenv ctx a uScrut tScrut alt
 = let  pp      = topEnvPlatform penv
        defs    = topEnvDataDefs penv
   in case alt of
        -- Match against the unit constructor.
        --  This is baked into the langauge and doesn't have a real name,
        --  so we need to handle it separately.
        AAlt (PData dc []) x
         | DaConUnit    <- dc
         -> do  xBody           <- convertExpX penv kenv tenv ctx x
                let dcTag       = DaConPrim (A.NameLitTag 0) A.tTag
                return  $ AAlt (PData dcTag []) xBody

        -- Match against literal unboxed values.
        AAlt (PData dc []) x
         | Just nCtor           <- takeNameOfDaCon dc
         , E.isNameLit nCtor
         -> do  dc'             <- convertDaCon kenv dc
                xBody1          <- convertExpX penv kenv tenv ctx x
                return  $ AAlt (PData dc' []) xBody1

        -- Match against user-defined algebraic data.
        AAlt (PData dc bsFields) x
         | Just nCtor   <- takeNameOfDaCon dc
         , Just ctorDef <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                -- Convert the scrutinee.
                uScrut'         <- convertValueU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let dcTag       = DaConPrim (A.NameLitTag iTag) A.tTag
                
                -- Get the address of the payload.
                bsFields'       <- mapM (convertRepableB kenv) bsFields

                -- Convert the right of the alternative, 
                -- with all all the pattern variables in scope.
                let tenv'       = Env.extends bsFields tenv 
                xBody1          <- convertExpX penv kenv tenv' ctx x

                -- Determine the prime region of the scrutinee.
                -- This is the region the associated Salt object is in.
                trPrime         <- saltPrimeRegionOfDataType kenv tScrut

                -- Wrap the body expression with let-bindings that bind
                -- each of the fields of the data constructor.
                xBody2          <- destructData pp a ctorDef uScrut' trPrime
                                        bsFields' xBody1

                return  $ AAlt (PData dcTag []) xBody2

        -- Default alternative.
        AAlt PDefault x
         -> do  x'      <- convertExpX penv kenv tenv ctx x 
                return  $ AAlt PDefault x'

        AAlt{}          
         -> throw ErrorInvalidAlt


---------------------------------------------------------------------------------------------------
-- | Convert a witness expression to Salt
--   TODO: Witness conversion is currently broken.
--         We need to handle conversion of witness types.
convertWitnessX
        :: Show a
        => KindEnv E.Name                   -- ^ Kind enviornment
        -> Witness (AnTEC a E.Name) E.Name  -- ^ Witness to convert.
        -> ConvertM a (Witness a A.Name)

convertWitnessX kenv ww
 = let down = convertWitnessX kenv
   in  case ww of
            WVar  a n     -> liftM  (WVar  $ annotTail a) (convertValueU n)
            WCon  a wc    -> liftM  (WCon  $ annotTail a) (convertWiConX kenv wc)
            WApp  a w1 w2 -> liftM2 (WApp  $ annotTail a) (down w1) (down w2)
            WJoin a w1 w2 -> liftM2 (WApp  $ annotTail a) (down w1) (down w2)
            WType a t     -> liftM  (WType $ annotTail a) (convertK t)


-- | Conert a witness constructor to Salt.
convertWiConX
        :: Show a
        => KindEnv E.Name               -- ^ Kind environment. 
        -> WiCon   E.Name               -- ^ Witness constructor to convert.
        -> ConvertM a (WiCon A.Name)    

convertWiConX kenv wicon            
 = case wicon of
        WiConBuiltin w
         -> return $ WiConBuiltin w

        WiConBound n t 
         -> liftM2 WiConBound (convertTypeU n) (convertRepableT kenv t)


---------------------------------------------------------------------------------------------------
-- | Convert a data constructor application to Salt.
convertCtorAppX 
        :: Show a
        => TopEnv                         -- ^ Top-level environment,
        -> KindEnv  E.Name                -- ^ Kind environment.
        -> TypeEnv  E.Name                -- ^ Type environment.
        -> AnTEC a  E.Name                -- ^ Annot from deconstructed app node.
        -> DaCon    E.Name                -- ^ Data constructor being applied.
        -> [Exp (AnTEC a E.Name) E.Name]  -- ^ Data constructor arguments.
        -> ConvertM a (Exp a A.Name)

convertCtorAppX penv kenv tenv (AnTEC tResult _ _ a) dc xsArgsAll
 -- Handle the unit constructor.
 | DaConUnit     <- dc
 = do    return  $ A.xAllocBoxed a A.rTop 0 (A.xNat a 0)

 -- Construct algebraic data.
 | Just nCtor    <- takeNameOfDaCon dc
 , Just ctorDef  <- Map.lookup nCtor $ dataDefsCtors (topEnvDataDefs penv)
 , Just dataDef  <- Map.lookup (dataCtorTypeName ctorDef) 
                 $  dataDefsTypes (topEnvDataDefs penv)
 = do   
        let pp           = topEnvPlatform penv

        -- Get the prime region variable.
        -- The prime region holds the outermost constructor of the object.
        trPrime          <- saltPrimeRegionOfDataType kenv tResult

        -- Split the constructor arguments into the type and value args.
        let xsArgsTypes  = [x | x@XType{} <- xsArgsAll]
        let xsArgsValues = drop (length xsArgsTypes) xsArgsAll

        -- Convert all the constructor arguments to Salt.
        xsArgsValues'    <- mapM (convertExpX penv kenv tenv ExpArg) 
                         $  xsArgsValues

        -- Determine the Salt type for each of the arguments.
        tsArgsValues'    <- mapM (saltDataTypeOfArgType kenv) 
                         $  map (annotType . annotOfExp) xsArgsValues

        constructData pp kenv tenv a
                dataDef ctorDef
                trPrime xsArgsValues' tsArgsValues'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorAppX _ _ _ _ _ _
        = throw $ ErrorMalformed "Invalid constructor application."


---------------------------------------------------------------------------------------------------
-- | Given an argument to a function or data constructor, either convert
--   it to the corresponding argument to use in the Salt program, or 
--   return Nothing which indicates it should be discarded.
convertOrDiscardSuperArgX
        :: Show a                       
        => TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Maybe (Exp a A.Name))

convertOrDiscardSuperArgX penv kenv tenv xx

        -- Region type arguments get passed through directly.
        | XType a t     <- xx
        , isRegionKind (annotType a)
        = do    t'      <- convertRegionT kenv t
                return  $ Just (XType (annotTail a) t')

        -- If we have a data type argument where the type is boxed, then we pass
        -- the region the corresponding Salt object is in.
        | XType a t     <- xx
        , isDataKind   (annotType a)
        , isBoxedRepType t
        = do    t'      <- saltPrimeRegionOfDataType kenv t
                return  $ Just (XType (annotTail a) t')

        -- Some type that we don't know how to convert to Salt.
        -- We don't handle type args with higher kinds.
        | XType{}       <- xx
        =       throw $ ErrorMalformed "Invalid type argument to super or ctor."

        -- Witness arguments are discarded.
        | XWitness{}    <- xx
        =       return  $ Nothing

        -- Expression arguments.
        | otherwise
        = do    x'      <- convertExpX penv kenv tenv ExpArg xx
                return  $ Just x'


-- | Although we ditch type arguments when applied to general functions,
--   we need to convert the ones applied directly to primops, 
--   as the primops are specified polytypically.
convertPrimArgX 
        :: Show a 
        => TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> ExpContext                   -- ^ What context we're converting in.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertPrimArgX penv kenv tenv ctx xx
 = case xx of
        XType a t
         -> do  t'      <- convertRepableT kenv t
                return  $ XType (annotTail a) t'

        XWitness a w
         -> do  w'      <- convertWitnessX kenv w
                return  $ XWitness (annotTail a) w'

        _ -> convertExpX penv kenv tenv ctx xx


---------------------------------------------------------------------------------------------------
-- | Convert a literal constructor to Salt.
--   These are values that have boxable index types like Bool# and Nat#.
convertLitCtorX
        :: a 
        -> DaCon E.Name
        -> ConvertM a (Exp a A.Name)

convertLitCtorX a dc
 | Just n        <- takeNameOfDaCon dc
 = case n of
        E.NameLitBool b         -> return $ A.xBool a b
        E.NameLitNat  i         -> return $ A.xNat  a i
        E.NameLitInt  i         -> return $ A.xInt  a i
        E.NameLitWord i bits    -> return $ A.xWord a i bits
        _                       -> throw $ ErrorMalformed "Invalid literal."

 | otherwise    
 = throw $ ErrorMalformed "Invalid literal."

