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

import DDC.Control.Monad.Check           (throw, evalCheck)
import qualified Data.Map                as Map
import Control.Monad
import Data.Maybe

import DDC.Base.Pretty

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


-- Module ---------------------------------------------------------------------
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
        tsExports'
                <- liftM Map.fromList
                $  mapM convertExportM 
                $  Map.toList 
                $  moduleExportTypes mm

        -- Convert signatures of imported functions.
        tsImports'
                <- liftM Map.fromList
                $  mapM convertImportM  
                $  Map.toList
                $  moduleImportTypes mm

        -- Convert the body of the module to Salt.
        let ntsImports  
                   = [(BName n t) | (n, (_, t)) <- Map.toList $ moduleImportTypes mm]
        let tenv'  = Env.extends ntsImports tenv
        
        x1         <- convertExpX ExpTop pp defs kenv tenv' 
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
                , moduleExportKinds    = Map.empty
                , moduleExportTypes    = tsExports'

                , moduleImportKinds    = A.runtimeImportKinds
                , moduleImportTypes    = Map.union A.runtimeImportTypes tsImports'

                  -- Data constructors and pattern matches should have been
                  -- flattenedinto primops, so we don't need the data type
                  -- definitions.
                , moduleDataDefsLocal  = Map.empty

                , moduleBody           = x2 }

        -- If this is the 'Main' module then add code to initialise the 
        -- runtime system. This will fail if given a Main module with no
        -- 'main' function.
        mm_init <- case initRuntime runConfig mm_salt of
                        Nothing   -> throw ErrorMainHasNoMain
                        Just mm'  -> return mm'

        return $ mm_init


-- | Convert an export spec.
convertExportM
        :: (E.Name, Type E.Name)                
        -> ConvertM a (A.Name, Type A.Name)

convertExportM (n, t)
 = do   n'      <- convertBindNameM n
        t'      <- convertRepableT Env.empty t
        return  (n', t')


-- | Convert an import spec.
convertImportM
        :: (E.Name, (QualName E.Name, Type E.Name))
        -> ConvertM a (A.Name, (QualName A.Name, Type A.Name))

convertImportM (n, (qn, t))
 = do   n'      <- convertBindNameM n
        qn'     <- convertQualNameM qn
        t'      <- convertRepableT Env.empty t
        return  (n', (qn', t'))


-- | Convert a qualified name.
convertQualNameM
        :: QualName E.Name 
        -> ConvertM a (QualName A.Name)

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
        -> DataDefs E.Name              -- ^ Data type definitions.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertExpX ctx pp defs kenv tenv xx
 = let downArgX     = convertExpX     ExpArg pp defs kenv tenv
       downPrimArgX = convertPrimArgX ExpArg pp defs kenv tenv
       downCtorAppX = convertCtorAppX pp defs kenv tenv
   in case xx of

        ---------------------------------------------------
        XVar _ UIx{}
         -> throw $ ErrorMalformed 
                  $ "Cannot convert program with anonymous value binders."

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertValueU u
                return  $  XVar a' u'

        XCon a u
         -> do  let a'  = annotTail a
                xx'     <- convertCtor pp defs kenv tenv a' u
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
                x'        <- convertExpX  ctx pp defs kenv' tenv x

                return $ XLAM a' b' x'

         | ExpFun       <- ctx
         -> do  let kenv'       = Env.extend b kenv
                convertExpX ctx pp defs kenv' tenv x

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
                        (convertExpX   ctx pp defs kenv tenv' x)

                Just UniverseWitness 
                 -> liftM3 XLam
                        (return $ annotTail a)
                        (convertRepableB kenv b)
                        (convertExpX   ctx pp defs kenv tenv' x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b
         | otherwise
         -> throw $ ErrorMalformed 
                  $ "Cannot convert XLam in this context " ++ show ctx


        ---------------------------------------------------
        -- Region application.
        --   These are passed through to the Salt language.
        XApp a1 x1 (XType a2@(AnTEC k _ _ _) t2)
         | isRegionKind k
         -> do  
                x1'     <- downArgX x1
                t2'     <- convertRegionT kenv t2
                return  $ XApp (annotTail a1) x1' 
                               (XType (annotTail a2) t2')


        ---------------------------------------------------
        -- Wrapping of pure values into boxed values.
        --   We fake-up a data-type declaration so we can use the same data layout
        --   code as for used-defined types.
        XApp a _ _
         | Just ( E.NamePrimCast E.PrimCastConvert
                , [XType _ tBIx, XType _ tBx, XCon _ u]) <- takeXPrimApps xx
         , isBoxableIndexType tBIx
         , isBoxedRepType     tBx
         , Just dt      <- makeDataTypeForBoxableIndexType tBIx
         , Just dc      <- makeDataCtorForBoxableIndexType tBIx
         -> do  
                let a'  = annotTail a
                xArg'   <- convertCtor   pp defs kenv tenv a' u
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
        -- Fully applied data constructor applications.
        -- TODO: handle user defined constructors.
        XApp a xa xb
         | (x1, xsArgs)         <- takeXApps1 xa xb
         , XCon _ dc            <- x1
         , Just tCon            <- takeTypeOfDaCon dc
         , length xsArgs == arityOfType tCon
         -> downCtorAppX a dc xsArgs


        ---------------------------------------------------
        -- Saturated application of a primitive operator.
        XApp a xa xb
         | (x1, xsArgs)           <- takeXApps1 xa xb
         , XVar _ (UPrim _ tPrim) <- x1

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
                return $ xApps (annotTail a) x1' xsArgs'


        ---------------------------------------------------
        -- Saturated application of a user-defined function.
        --  This does not cover application of primops, the above case should fire for these.
        --
        --  ISSUE #283: Lite to Salt transform doesn't check for partial application
        --  TODO: check the thing being applied is a named super defined at top-level.
        --
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, xsArgs) <- takeXApps1 xa xb
         
         -- The thing being applied is a named function.
         , XVar _ UName{} <- x1

         -- The function is saturated.
         , length xsArgs == arityOfType (annotType $ annotOfExp x1)

         -> do  
                let keepArg x
                        = case x of
                           XType a _   -> isRegionKind (annotType a)
                           XWitness{}  -> False
                           _           -> True 

                x1'     <- downArgX x1
                xsArgs' <- mapM downArgX 
                        $  filter keepArg xsArgs
                
                return  $ xApps a' x1' xsArgs'


        ---------------------------------------------------
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
         -> throw $ ErrorNotNormalized "Unexpected let-expression."


        ---------------------------------------------------
        -- Match against literal unboxed values.
        --  The branch is against the literal value itself.
        XCase (AnTEC _ _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon (TyConBound (UPrim nType _) _)  <- tScrut
         , E.NamePrimTyCon _                    <- nType
         -> do  xScrut' <- convertExpX ExpArg pp defs kenv tenv xScrut
                alts'   <- mapM (convertAlt (min ctx ExpBody) pp 
                                        defs kenv tenv a' uScrut tScrut) 
                                alts
                return  $  XCase a' xScrut' alts'


        ---------------------------------------------------
        -- Match against finite algebraic data.
        --   The branch is against the constructor tag.
        XCase (AnTEC tX _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | TCon _ : _   <- takeTApps tScrut
         , isSomeRepType tScrut
         -> do  x'      <- convertExpX     ExpArg pp defs kenv tenv xScrut
                tX'     <- convertRepableT kenv tX
                alts'   <- mapM (convertAlt (min ctx ExpBody) pp 
                                        defs kenv tenv a' uScrut tScrut) 
                                alts

                let asDefault
                        | any isPDefault [p | AAlt p _ <- alts]   
                        = []

                        | otherwise     
                        = [AAlt PDefault (A.xFail a' tX')]

                tScrut'    <- convertRepableT kenv tScrut
                let tPrime = fromMaybe A.rTop
                           $ takePrimeRegion tScrut'

                return  $ XCase a' (A.xGetTag a' tPrime x') 
                        $ alts' ++ asDefault


        ---------------------------------------------------
        -- Trying to matching against something that isn't primitive or
        -- algebraic data.
        XCase{} 
         -> throw $ ErrorNotNormalized "Invalid case expression."


        ---------------------------------------------------
        -- Casts.
        XCast _ _ x
         -> convertExpX (min ctx ExpBody) pp defs kenv tenv x


        -- We shouldn't find any naked types.
        -- These are handled above in the XApp case.
        XType{}
          -> throw $ ErrorNotNormalized "Unexpected type argument."


        -- We shouldn't find any naked types.
        -- TODO: handle these in the XApp case above.
        XWitness{}
          -> throw $ ErrorNotNormalized "Unexpected witness expression."


        _ -> throw $ ErrorNotNormalized 
                   $ "Cannot convert expression.\n"
                   ++ (renderIndent $ ppr xx)



-- | Although we ditch type arguments when applied to general functions,
--   we need to convert the ones applied directly to primops, 
--   as the primops are specified polytypically.
convertPrimArgX 
        :: Show a 
        => ExpContext                   -- ^ What context we're converting in.
        -> Platform                     -- ^ Platform specification.
        -> DataDefs E.Name              -- ^ Data type definitions.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertPrimArgX ctx pp defs kenv tenv xx
 = case xx of
        XType a t
         -> do  t'      <- convertRepableT kenv t
                return  $ XType (annotTail a) t'

        XWitness a w
         -> do  w'      <- convertWitnessX kenv w
                return  $ XWitness (annotTail a) w'

        _ -> convertExpX ctx pp defs kenv tenv xx


-------------------------------------------------------------------------------
-- | Convert a let-binding to Salt.
convertLetsX 
        :: Show a 
        => Platform                     -- ^ Platform specification.
        -> DataDefs E.Name              -- ^ Data type definitions.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Lets (AnTEC a E.Name) E.Name -- ^ Expression to convert.
        -> ConvertM a (Lets a A.Name)

convertLetsX pp defs kenv tenv lts
 = case lts of
        LRec bxs
         -> do  let tenv'    = Env.extends (map fst bxs) tenv
                let (bs, xs) = unzip bxs
                bs'          <- mapM (convertValueB kenv) bs
                xs'          <- mapM (convertExpX ExpFun pp defs kenv tenv') xs
                return  $ LRec $ zip bs' xs'

        LLet b x1
         -> do  let tenv'    = Env.extend b tenv
                b'           <- convertValueB kenv b
                x1'          <- convertExpX   ExpBind pp defs kenv tenv' x1
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


-------------------------------------------------------------------------------
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


-------------------------------------------------------------------------------
-- | Convert a data constructor application to Salt.
convertCtorAppX 
        :: Show a
        => Platform                     -- ^ Platform specification.
        -> DataDefs E.Name              -- ^ Data type definitions.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> AnTEC a  E.Name              -- ^ Annot from deconstructed app node.
        -> DaCon    E.Name              -- ^ Data constructor being applied.
        -> [Exp (AnTEC a E.Name) E.Name]
        -> ConvertM a (Exp a A.Name)

convertCtorAppX pp defs kenv tenv (AnTEC _ _ _ a) dc xsArgs
        -- Handle the unit constructor.
        | DaConUnit     <- dc
        = do    return  $ A.xAllocBoxed a A.rTop 0 (A.xNat a 0)

        -- Pass through unboxed literals.
        | Just (E.NameLitBool b)        <- takeNameOfDaCon dc
        , []                            <- xsArgs
        = return $ A.xBool a b

        | Just (E.NameLitNat i)         <- takeNameOfDaCon dc
        , []                            <- xsArgs
        = return $ A.xNat  a i

        | Just (E.NameLitInt i)         <- takeNameOfDaCon dc
        , []                            <- xsArgs
        = return $ A.xInt  a i

        | Just (E.NameLitWord i bits)   <- takeNameOfDaCon dc
        , []                            <- xsArgs
        = return $ A.xWord a i bits

        -- Construct algbraic data that has a finite number of data constructors.
        | Just nCtor    <- takeNameOfDaCon dc
        , Just ctorDef  <- Map.lookup nCtor $ dataDefsCtors defs
        , Just dataDef  <- Map.lookup (dataCtorTypeName ctorDef) 
                        $ dataDefsTypes defs
        = do    
                -- Get the prime region variable that holds the outermost
                -- constructor. For types like Unit, there is no prime region,
                -- so put them in the top-level region of the program.
                rPrime
                 <- case xsArgs of
                     [] 
                      -> return A.rTop

                     XType _ (TVar u) : _
                      | Just tu      <- Env.lookup u kenv
                      -> if isRegionKind tu
                          then do u'      <- convertTypeU u
                                  return  $ TVar u'
                          else return A.rTop

                     _ -> throw 
                       $ ErrorMalformed "Prime region variable is not in scope."


                -- Convert the types of each field.
                let makeFieldType x
                        = let a' = annotOfExp x 
                          in  convertRepableT kenv (annotType a')

                xsArgs' <- mapM (convertExpX ExpArg pp defs kenv tenv) xsArgs
                tsArgs' <- mapM makeFieldType xsArgs
                constructData pp kenv tenv a
                        dataDef ctorDef
                        rPrime xsArgs' tsArgs'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorAppX _ _ _ _ _ _nCtor _xsArgs
        = throw $ ErrorMalformed "Invalid constructor application."


-- Alt ------------------------------------------------------------------------
-- | Convert a Lite alternative to Salt.
convertAlt 
        :: Show a
        => ExpContext
        -> Platform                     -- ^ Platform specification.
        -> DataDefs E.Name              -- ^ Data type declarations.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> a                            -- ^ Annotation from case expression.
        -> Bound E.Name                 -- ^ Bound of scrutinee.
        -> Type  E.Name                 -- ^ Type  of scrutinee
        -> Alt (AnTEC a E.Name) E.Name  -- ^ Alternative to convert.
        -> ConvertM a (Alt a A.Name)

convertAlt ctx pp defs kenv tenv a uScrut tScrut alt
 = case alt of
        -- Default alternative.
        AAlt PDefault x
         -> do  x'      <- convertExpX ctx pp defs kenv tenv x
                return  $ AAlt PDefault x'

        -- Match against literal unboxed values.
        AAlt (PData dc []) x
         | Just nCtor           <- takeNameOfDaCon dc
         , case nCtor of
                E.NameLitBool{} -> True
                E.NameLitNat{}  -> True
                E.NameLitInt{}  -> True
                E.NameLitWord{} -> True
                _               -> False

         -> do  dc'     <- convertDaCon kenv dc
                xBody1  <- convertExpX  ctx pp defs kenv tenv x
                return  $ AAlt (PData dc' []) xBody1

        -- Match against the unit constructor.
        --  This is baked into the langauge and doesn't have a real name,
        --  so we need to handle it separately.
        AAlt (PData dc []) x
         | DaConUnit    <- dc
         -> do  xBody           <- convertExpX ctx pp defs kenv tenv x
                let dcTag       = DaConPrim (A.NameLitTag 0) A.tTag
                return  $ AAlt (PData dcTag []) xBody

        -- Match against algebraic data with a finite number
        -- of data constructors.
        AAlt (PData dc bsFields) x
         | Just nCtor   <- takeNameOfDaCon dc
         , Just ctorDef <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                let tenv'       = Env.extends bsFields tenv 
                uScrut'         <- convertValueU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let dcTag       = DaConPrim (A.NameLitTag iTag) A.tTag
                
                -- Get the address of the payload.
                bsFields'       <- mapM (convertRepableB kenv) bsFields

                -- Convert the right of the alternative.
                xBody1          <- convertExpX ctx pp defs kenv tenv' x

                -- Add let bindings to unpack the constructor.
                tScrut'         <- convertRepableT kenv tScrut
                let Just trPrime = takePrimeRegion tScrut'
                xBody2           <- destructData pp a ctorDef uScrut' trPrime
                                                 bsFields' xBody1
                return  $ AAlt (PData dcTag []) xBody2

        AAlt{}          
         -> throw ErrorInvalidAlt


-- Data Constructor -----------------------------------------------------------
-- | Expand out code to build a data constructor.
--   TODO: fold this into convertCtorAppX
convertCtor 
        :: Show a
        => Platform             -- ^ Platform specification.
        -> DataDefs E.Name      -- ^ Data type definitions.
        -> KindEnv  E.Name      -- ^ Kind environment.
        -> TypeEnv  E.Name      -- ^ Type environment.
        -> a                    -- ^ Annotation to attach to exp nodes.
        -> DaCon    E.Name      -- ^ Data constructor to convert.
        -> ConvertM a (Exp a A.Name)

convertCtor pp defs kenv tenv a dc
 | DaConUnit    <- dc
 =      return $ A.xAllocBoxed a A.rTop 0 (A.xNat a 0)

 | Just n       <- takeNameOfDaCon dc
 = case n of
        -- Literal values.
        E.NameLitBool v         -> return $ A.xBool a v
        E.NameLitNat  i         -> return $ A.xNat  a i
        E.NameLitInt  i         -> return $ A.xInt  a i
        E.NameLitWord i bits    -> return $ A.xWord a i bits

        -- A Zero-arity data constructor.
        nCtor
         | Just ctorDef         <- Map.lookup nCtor $ dataDefsCtors defs
         , Just dataDef         <- Map.lookup (dataCtorTypeName ctorDef) 
                                $ dataDefsTypes defs
         -> do  -- Put zero-arity data constructors in the top-level region.
                let rPrime      = A.rTop
                constructData pp kenv tenv a dataDef ctorDef rPrime [] []

        _ -> throw $ ErrorMalformed "Invalid constructor."

 | otherwise
 = throw $ ErrorMalformed "Invalid constructor."

