-- | Conversion of Disciple Lite to Disciple Salt.
module DDC.Core.Tetra.Convert.Exp
        ( TopEnv        (..)
        , ExpContext    (..)
        , convertExpX)
where
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Data
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Core.Transform.LiftX
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Env       as A
import qualified DDC.Core.Salt.Compounds as A

import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Env                      (KindEnv, TypeEnv)
import qualified DDC.Type.Env            as Env

import Control.Monad
import Data.Maybe
import DDC.Base.Pretty
import DDC.Control.Monad.Check           (throw)
import Data.Set                          (Set)
import qualified Data.Map                as Map
import qualified Data.Set                as Set


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


-- | Get the value arity of a supercombinator. 
--   This is how many data arguments it needs when we call it.
superDataArity :: TopEnv -> TypeEnv E.Name -> Bound E.Name -> Maybe Int
superDataArity env tenv u
        | UName n  <- u
        , Just  t  <- Env.lookup u tenv
        , Set.member n (topEnvSupers env)
        = Just $ dataArityOfType t

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
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
         -> throw $ ErrorUnsupported xx
                  $ vcat [ text "Cannot convert program with anonymous value binders."
                         , text "The program must be namified before conversion." ]

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertDataU u
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
         -- then the type lambda in Tetra is converted to a region lambda in
         -- Salt which binds the region the object is in.
         | ExpFun       <- ctx
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

         -- Erase higher kinded type lambdas.
         | ExpFun       <- ctx
         , Just _       <- takeKFun $ typeOfBind b
         -> do  let kenv'       = Env.extend b kenv
                convertExpX penv kenv' tenv ctx x

         -- A type abstraction that we can't convert to Salt.
         | otherwise
         -> throw $ ErrorUnsupported xx
                  $ vcat [ text "Cannot convert type abstraction in this context."
                         , text "The program must be lambda-lifted before conversion." ]


        ---------------------------------------------------
        -- Function abstractions can only appear at the top-level of a fucntion.
        XLam a b x
         | ExpFun       <- ctx
         -> let tenv'   = Env.extend b tenv
            in case universeFromType1 kenv (typeOfBind b) of
                Just UniverseData
                 -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertValueB defs kenv b) 
                        (convertExpX penv kenv tenv' ctx x)

                Just UniverseWitness 
                 -> liftM3 XLam
                        (return $ annotTail a)
                        (convertValueB defs kenv b)
                        (convertExpX penv kenv tenv' ctx x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b
         | otherwise
         -> throw $ ErrorUnsupported xx
                  $ vcat [ text "Cannot convert function abstraction in this context."
                         , text "The program must be lambda-lifted before conversion." ]


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
                tBIx'   <- convertIndexT   tBIx
                tBx'    <- convertValueT defs kenv tBx

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
                tBx'    <- convertValueT defs kenv tBx

                x'      <- destructData pp a' dc
                                (UIx 0) A.rTop 
                                [BAnon tBIx'] (XVar a' (UIx 0))

                return  $ XLet a' (LLet (BAnon tBx') (liftX 1 xArg'))
                                  x'


        ---------------------------------------------------
        -- Reify a top-level super.
        --  TODO: Check that we're only reifying functions that will have
        --        the standard calling convention.
        XApp (AnTEC _t _ _ a)  xa xb
         | (x1, [XType _ t1, XType _ t2, xF]) <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)  <- x1
         , E.NameOpFun E.OpFunCReify    <- nPrim
         , XVar _ uF                    <- xF
         -> do
                xF'     <- downArgX xF
                tF'     <- convertRepableT defs kenv (tFun t1 t2)
                let Just arity = superDataArity penv tenv uF

                return  $ A.xAllocThunk a A.rTop 
                                (xConvert a A.tAddr tF' xF')
                                (A.xNat a $ fromIntegral arity)
                                (A.xNat a 0)


        ---------------------------------------------------
        -- Curry arguments onto a reified function.
        --   This works for both the 'curryN#' and 'extendN#' primops,
        --   as they differ only in the Tetra-level closure type.
        XApp (AnTEC _t _ _ a) xa xb
         | (x1, xs)                     <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)  <- x1

         , Just nArgs   
            <- case nPrim of 
                E.NameOpFun (E.OpFunCurry   nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCCurry  nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCExtend nArgs) -> Just nArgs
                _                                  -> Nothing

         , tsArg              <- [tArg | XType _ tArg <- take nArgs xs]
         , (xThunk : xsArg)   <- drop (nArgs + 1) xs
         , nArgs == length xsArg
         -> do  
                xThunk'         <- downArgX xThunk
                xsArg'          <- mapM downArgX xsArg
                tsArg'          <- mapM (convertValueT defs kenv) tsArg
                let bObject     = BAnon (A.tPtr A.rTop A.tObj)
                let bAvail      = BAnon A.tNat

                return 
                 $ XLet  a (LLet bObject 
                                 (A.xExtendThunk     a A.rTop A.rTop xThunk' 
                                        (A.xNat a $ fromIntegral nArgs)))
                 $ XLet  a (LLet bAvail
                                 (A.xAvailOfThunk    a A.rTop xThunk'))

                 $ xLets a [LLet (BNone A.tVoid)
                                 (A.xSetFieldOfThunk a A.rTop 
                                        (XVar a (UIx 1))                 -- new thunk
                                        (XVar a (UIx 0))                 -- base index
                                        (A.xNat a ix)                    -- offset
                                        (xTakePtr a tPrime A.tObj xArg)) -- value
                                 | ix   <- [0..]
                                 | xArg <- xsArg'
                                 | tArg <- tsArg'
                                 , let tPrime   = fromMaybe A.rTop
                                                $ takePrimeRegion tArg ]

                 $ XVar a (UIx 1)


        ---------------------------------------------------
        -- Apply a thunk.
        XApp (AnTEC _t _ _ a) xa xb
         | (x1, xs)                           <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)        <- x1
         , Just nArgs
            <- case nPrim of
                E.NameOpFun (E.OpFunApply  nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCApply nArgs) -> Just nArgs
                _                                 -> Nothing

         , tsArg                <- [tArg | XType _ tArg <- take nArgs xs]
         , XType _ tResult : _  <- drop  nArgs xs
         , xF : xsArgs          <- drop (nArgs + 1) xs
         -> do
                -- Functional expression.
                xF'             <- downArgX xF

                -- Arguments and theit ypes.
                xsArg'          <- mapM downArgX xsArgs
                tsArg'          <- mapM (convertValueT defs kenv) tsArg

                -- Result and its type.
                tResult'        <- convertValueT defs kenv tResult
                let tPrimeResult' = fromMaybe A.rTop $ takePrimeRegion tResult'

                -- Evaluate a thunk, returning the resulting Addr#, 
                -- then cast it back to a pointer of the appropriate type
                return  $ xMakePtr a tPrimeResult' A.tObj
                        $ A.xApplyThunk a nArgs 
                        $   [ xTakePtr a A.rTop A.tObj xF' ]
                         ++ [ xTakePtr a tPrime A.tObj xArg'
                                | xArg'         <- xsArg'
                                | tArg'         <- tsArg'
                                , let tPrime    = fromMaybe A.rTop
                                                $ takePrimeRegion tArg' ]

        
        ---------------------------------------------------
        -- Saturated application of a primitive data constructor,
        --   including the Unit data constructor.
        --   The types of these are directly attached.
        XApp a xa xb
         | (x1, xsArgs)         <- takeXApps1 xa xb
         , XCon _ dc            <- x1
         , Just tCon            <- takeTypeOfDaCon dc
         -> if -- Check that the constructor is saturated.
               length xsArgs == arityOfType tCon
               then downCtorAppX a dc xsArgs
               else throw $ ErrorUnsupported xx
                     $ text "Partial application of primitive data constructors is not supported."


        -- Fully applied user-defined data constructor application.
        --   The types of these are in the defs list.
        XApp a xa xb
         | (x1, xsArgs   )          <- takeXApps1 xa xb
         , XCon _ dc@(DaConBound n) <- x1
         , Just dataCtor            <- Map.lookup n (dataDefsCtors defs)
         -> if -- Check that the constructor is saturated.
               length xsArgs 
                       == length (dataCtorTypeParams dataCtor)
                       +  length (dataCtorFieldTypes dataCtor)
               then downCtorAppX a dc xsArgs
               else throw $ ErrorUnsupported xx
                     $ text "Partial application of user-defined data constructors is not supported."


        ---------------------------------------------------
        -- Saturated application of a primitive operator.
        XApp a xa xb
         | (x1, xsArgs)               <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim tPrim) <- x1

         -- All the value arguments have representatable types.
         , all isSomeRepType
                $  map (annotType . annotOfExp)
                $  filter (not . isXType) xsArgs

         -- The result is representable.
         , isSomeRepType (annotType a)

         -> if -- Check that the primop is saturated.
             length xsArgs == arityOfType tPrim
             then do
                x1'     <- downArgX x1
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

             else throw $ ErrorUnsupported xx
                   $ text "Partial application of primitive operators is not supported."


        ---------------------------------------------------
        -- Saturated application of a top-level supercombinator or imported function.
        --  This does not cover application of primops, the above case should
        --  fire for those.
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
                        $  mapM (convertOrDiscardSuperArgX xx penv kenv tenv) xsArgs
                        
                return  $ xApps a' x1' xsArgs'


        ---------------------------------------------------
        -- Application of some function that is not a top-level supercombinator
        -- or imported function. 
        XApp _ xa xb
         | (x1, _xsArgs) <- takeXApps1 xa xb

         -- The thing being applied is a named function but is not defined
         -- at top level, or imported directly.
         , XVar _ (UName n) <- x1
         , not $ Set.member n (topEnvSupers       penv)
         , not $ Set.member n (topEnvImportValues penv)
         -> throw $ ErrorUnsupported xx
                  $ text "Higher order functions are not yet supported."

        
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
         -> throw $ ErrorUnsupported xx 
                  $ vcat [ text "Cannot convert a let-expression in this context."
                         , text "The program must be a-normalized before conversion." ]


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
                x'      <- convertExpX   penv kenv tenv ExpArg xScrut
                tX'     <- convertValueT defs kenv tX

                tScrut' <- convertValueT defs kenv tScrut
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
         -> throw $ ErrorUnsupported xx  
                  $ text "Unsupported form of case expression" 

        ---------------------------------------------------
        -- Casts.
        XCast _ _ x
         -> convertExpX penv kenv tenv (min ctx ExpBody) x


        -- We shouldn't find any naked types.
        -- These are handled above in the XApp case.
        XType{}
          -> throw $ ErrorMalformed "Found a naked type argument."


        -- We shouldn't find any naked witnesses.
        XWitness{}
          -> throw $ ErrorMalformed "Found a naked witness."

        -- Expression can't be converted.
        _ -> throw $ ErrorUnsupported xx 
                   $ text "Unrecognised expression form."


xConvert :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xConvert a t1 t2 x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimCast $ A.PrimCastConvert)
                                  (A.typeOfPrimCast A.PrimCastConvert)))
                  [ XType a t1, XType a t2, x1 ]


xTakePtr :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xTakePtr a tR tA x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimStore A.PrimStoreTakePtr)
                                  (A.typeOfPrimStore A.PrimStoreTakePtr)))
                  [ XType a tR, XType a tA, x1 ]


xMakePtr :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xMakePtr a tR tA x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimStore A.PrimStoreMakePtr)
                                  (A.typeOfPrimStore A.PrimStoreMakePtr)))
                  [ XType a tR, XType a tA, x1 ]



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
 = let defs     = topEnvDataDefs penv
   in case lts of
        LRec bxs
         -> do  let tenv'    = Env.extends (map fst bxs) tenv
                let (bs, xs) = unzip bxs

                -- All the recursive bindings must be functional values, 
                -- so we use convertDataB here instead of convertValueB.
                bs'          <- mapM (convertDataB defs kenv) bs                
                xs'          <- mapM (convertExpX penv kenv tenv' ExpFun) xs
                return  $ LRec $ zip bs' xs'

        LLet b x1
         -> do  let tenv'    = Env.extend b tenv
                b'           <- convertValueB defs kenv b
                x1'          <- convertExpX   penv kenv tenv' ExpBind x1
                return  $ LLet b' x1'

        LPrivate b mt bs
         -> do  b'           <- mapM convertTypeB b
                let kenv'    = Env.extends b kenv
                
                bs'          <- mapM (convertCapabilityB kenv') bs
                mt'          <- case mt of
                                 Nothing -> return Nothing
                                 Just t  -> liftM Just $ convertRegionT kenv t
                return  $ LPrivate b' mt' bs'
  
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
         -> do  dc'             <- convertDaCon defs kenv dc
                xBody1          <- convertExpX penv kenv tenv ctx x
                return  $ AAlt (PData dc' []) xBody1

        -- Match against user-defined algebraic data.
        AAlt (PData dc bsFields) x
         | Just nCtor   <- takeNameOfDaCon dc
         , Just ctorDef <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                -- Convert the scrutinee.
                uScrut'         <- convertDataU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let dcTag       = DaConPrim (A.NameLitTag iTag) A.tTag
                
                -- Get the address of the payload.
                bsFields'       <- mapM (convertValueB defs kenv) bsFields       

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
        => Exp (AnTEC a E.Name) E.Name  -- ^ Overall application expression, for debugging.
        -> TopEnv                       -- ^ Top-level environment.
        -> KindEnv  E.Name              -- ^ Kind environment.
        -> TypeEnv  E.Name              -- ^ Type environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Maybe (Exp a A.Name))

convertOrDiscardSuperArgX xxApp penv kenv tenv xx

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

        -- Drop effect arguments.
        | XType a _     <- xx
        , isEffectKind (annotType a)
        =       return Nothing

        -- Some type that we don't know how to convert to Salt.
        -- We don't handle type args with higher kinds.
        -- See [Note: Salt conversion for higher kinded type arguments]
        | XType{}       <- xx
        = throw $ ErrorUnsupported xx
                $ vcat [ text "Unsupported type argument to function or constructor."
                       , text "In particular, we don't yet handle higher kinded type arguments."
                       , empty
                       , text "See [Note: Salt conversion for higher kinded type arguments] in"
                       , text "the implementation of the Tetra to Salt conversion." 
                       , empty
                       , text "with application: " <+> ppr xxApp ]

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
 = let defs     = topEnvDataDefs penv
   in case xx of
        XType a t
         -> do  t'      <- convertValueT defs kenv t
                return  $ XType (annotTail a) t'

        XWitness{}
         -> throw $ ErrorUnsupported xx
                  $ text "Witness expressions are not part of the Tetra language."

        _ -> convertExpX penv kenv tenv ctx xx


---------------------------------------------------------------------------------------------------
-- | Convert a literal constructor to Salt.
--   These are values that have boxable index types like Bool# and Nat#.
convertLitCtorX
        :: a                            -- ^ Annot from deconstructed XCon node.
        -> DaCon E.Name                 -- ^ Data constructor of literal.
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


---------------------------------------------------------------------------------------------------
-- [Note: Salt conversion for higher kinded type arguments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Converting functions that use higher kinded types to Salt is problematic
-- because we can't directly see what region is being used to represent
-- each object.
--
--   data List (r : Region) (a : Data) where ...
--
--   idf [c : Data ~> Data] [a : Data] (x : c a) : Nat# ...
--
--   f = ... idf [List r1] [Nat] (...)
--
-- At the call-site, the value argument to idf is in region r1, but that
-- information is not available when converting the body of 'idf'.
-- When converting the body of 'idf' we can't assume the value bound to 
-- 'x' is in rTop.
--
-- We need some simple subtyping in region types, to have a DontKnow region
-- that can be used to indicate that the region an object is in is unknown.
--
-- For now we just don't convert functions using higher kinded types, 
-- and leave this to future work. Higher kinding isn't particularly 
-- useful without a type clasing system with constructor classes,
-- so we'll fix it later.
--
