-- | Conversion of Disciple Lite to Disciple Salt.
module DDC.Core.Tetra.Convert.Exp
        (convertExp)
where
import DDC.Core.Tetra.Convert.Exp.Ctor
import DDC.Core.Tetra.Convert.Exp.PrimCall
import DDC.Core.Tetra.Convert.Exp.PrimArith
import DDC.Core.Tetra.Convert.Exp.PrimBoxing
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A

import DDC.Type.Universe
import DDC.Type.DataDef
import Control.Monad
import Data.Maybe
import DDC.Base.Pretty
import DDC.Control.Monad.Check           (throw)
import qualified Data.Map                as Map
import qualified Data.Set                as Set


-- | Convert the body of a supercombinator to Salt.
convertExp 
        :: Show a 
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context                      -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertExp ectx ctx xx
 = let defs         = contextDataDefs    ctx
       kenv         = contextKindEnv     ctx
       convertX     = contextConvertExp  ctx
       convertA     = contextConvertAlt  ctx
       convertLts   = contextConvertLets ctx
       downArgX     = convertX           ExpArg ctx 
       downCtorApp  = convertCtorApp     ctx
   in case xx of

        ---------------------------------------------------
        XVar _ UIx{}
         -> throw $ ErrorUnsupported xx
          $ vcat [ text "Cannot convert program with anonymous value binders."
                 , text "The program must be namified before conversion." ]

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertValueU u
                return  $  XVar a' u'


        ---------------------------------------------------
        -- Unapplied data constructor.
        XCon a dc
         -> do  xx'     <- downCtorApp a dc []
                return  xx'


        ---------------------------------------------------
        -- Type lambdas can only appear at the top-level of a function.
        --   We keep region lambdas but ditch the others. Polymorphic values
        --   are represented in generic boxed form, so we never need to 
        --   build a type abstraction of some other kind.
        XLAM a b x
         | ExpFun       <- ectx
         , isRegionKind $ typeOfBind b
         -> do  let a'    =  annotTail a
                b'        <- convertTypeB b

                let ctx'  =  extendKindEnv b ctx
                x'        <- convertExp  ectx ctx' x

                return $ XLAM a' b' x'

         -- When a function is fully polymorphic in some boxed data type,
         -- then the type lambda in Tetra is converted to a region lambda in
         -- Salt which binds the region the object is in.
         | ExpFun       <- ectx
         , BName (E.NameVar str) k <- b
         , isDataKind k
         , str'         <- str ++ "$r"
         , b'           <- BName (A.NameVar str') kRegion
         -> do  let a'   = annotTail a
                
                let ctx' = extendKindEnv b ctx 
                x'      <- convertExp   ectx ctx' x

                return $ XLAM a' b' x'

         -- Erase effect lambdas.
         | ExpFun       <- ectx
         , isEffectKind $ typeOfBind b
         -> do  let ctx' = extendKindEnv b ctx
                convertX ectx ctx' x

         -- Erase higher kinded type lambdas.
         | ExpFun       <- ectx
         , Just _       <- takeKFun $ typeOfBind b
         -> do  let ctx' = extendKindEnv b ctx
                convertX ectx ctx' x

         -- A type abstraction that we can't convert to Salt.
         | otherwise
         -> throw $ ErrorUnsupported xx
          $ vcat [ text "Cannot convert type abstraction in this context."
                 , text "The program must be lambda-lifted before conversion." ]


        ---------------------------------------------------
        -- Function abstractions can only appear at the top-level of a fucntion.
        XLam a b x
         | ExpFun       <- ectx
         -> let ctx'    = extendTypeEnv b ctx
            in case universeFromType1 kenv (typeOfBind b) of
                Just UniverseData
                 -> liftM3 XLam (return $ annotTail a) 
                                (convertValueB (typeContext ctx) b) 
                                (convertX      ectx ctx' x)

                Just UniverseWitness 
                 -> liftM3 XLam (return $ annotTail a)
                                (convertValueB (typeContext ctx) b)
                                (convertX      ectx ctx' x)

                _  -> throw $ ErrorMalformed 
                            $ "Invalid universe for XLam binder: " ++ show b
         | otherwise
         -> throw $ ErrorUnsupported xx
          $ vcat [ text "Cannot convert function abstraction in this context."
                 , text "The program must be lambda-lifted before conversion." ]

       
        ---------------------------------------------------
        -- Fully applied primitive data constructor.
        --  The type of the constructor is attached directly to this node of the AST.
        --  The data constructor must be fully applied. Partial applications of data 
        --  constructors that appear in the source language need to be eta-expanded
        --  before Tetra -> Salt conversion.
        XApp a xa xb
         | (x1, xsArgs)         <- takeXApps1 xa xb
         , XCon _ dc            <- x1
         , Just tCon            <- takeTypeOfDaCon dc
         -> if length xsArgs == arityOfType tCon
               then downCtorApp a dc xsArgs
               else throw $ ErrorUnsupported xx
                     $ text "Cannot convert partially applied data constructor."


        -- Fully applied user-defined data constructor application.
        --  The type of the constructor is retrieved in the data defs list.
        --  The data constructor must be fully applied. Partial applications of data 
        --  constructors that appear in the source language need to be eta-expanded
        --  before Tetra -> Salt conversion.
        XApp a xa xb
         | (x1, xsArgs   )          <- takeXApps1 xa xb
         , XCon _ dc@(DaConBound n) <- x1
         , Just dataCtor            <- Map.lookup n (dataDefsCtors defs)
         -> if length xsArgs 
                       == length (dataCtorTypeParams dataCtor)
                       +  length (dataCtorFieldTypes dataCtor)
               then downCtorApp a dc xsArgs
               else throw $ ErrorUnsupported xx
                     $ text "Cannot convert partially applied data constructor."


        ---------------------------------------------------
        -- Conversions for primitive operators are defined separately.
        XApp{}
         | Just makeX   <- convertPrimBoxing ectx ctx xx -> makeX
         | Just makeX   <- convertPrimCall   ectx ctx xx -> makeX
         | Just makeX   <- convertPrimArith  ectx ctx xx -> makeX


        ---------------------------------------------------
        -- Saturated application of a top-level supercombinator or imported function.
        --  This does not cover application of primops, those are handled by one 
        --  of the above cases.
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, xsArgs) <- takeXApps1 xa xb
         
         -- The thing being applied is a named function that is defined
         -- at top-level, or imported directly.
         , XVar _ (UName n) <- x1
         ,   Set.member n (contextSupers  ctx)
          || Set.member n (contextImports ctx)

         -- The function is saturated.
         , length xsArgs == arityOfType (annotType $ annotOfExp x1)

         -> do  -- Convert the functional part.
                x1'     <- downArgX x1

                -- Convert the arguments.
                -- Effect type and witness arguments are discarded here.
                xsArgs' <- liftM catMaybes 
                        $  mapM (convertOrDiscardSuperArgX xx ctx) xsArgs
                        
                return  $ xApps a' x1' xsArgs'

        
        ---------------------------------------------------
        -- let-expressions.
        XLet a lts x2
         | ectx <= ExpBind
         -> do  -- Convert the bindings.
                lts'            <- convertLts ctx lts

                -- Convert the body of the expression.
                let (bs1, bs0)  = bindsOfLets lts
                let ctx1        = extendsKindEnv bs1 ctx
                let ctx2        = extendsTypeEnv bs0 ctx1
                x2'             <- convertX ExpBody ctx2 x2

                return $ XLet (annotTail a) lts' x2'

        XLet{}
         -> throw $ ErrorUnsupported xx 
         $ vcat [ text "Cannot convert a let-expression in this context."
                , text "The program must be a-normalized before conversion." ]


        ---------------------------------------------------
        -- Match against literal unboxed values.
        --  The branch is against the literal value itself.
        XCase (AnTEC _ _ _ a') xScrut@(XVar (AnTEC tScrut _ _ _) uScrut) alts
         | isUnboxedRepType tScrut
         -> do  
                -- Convert the scrutinee.
                xScrut' <- convertX ExpArg ctx xScrut

                -- Convert the alternatives.
                alts'   <- mapM (convertA a' uScrut tScrut 
                                          (min ectx ExpBody) ctx) 
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
                x'      <- convertX      ExpArg ctx xScrut
                tX'     <- convertValueT (typeContext ctx) tX

                tScrut' <- convertValueT (typeContext ctx) tScrut
                let tPrime = fromMaybe A.rTop
                           $ takePrimeRegion tScrut'

                -- Convert alternatives.
                alts'   <- mapM (convertA a' uScrut tScrut 
                                          (min ectx ExpBody) ctx)
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
         $ text "Unsupported case expression form." 


        ---------------------------------------------------
        -- Type casts
        XCast _ _ x
         -> convertX (min ectx ExpBody) ctx x


        ---------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Given an argument to a function or data constructor, either convert
--   it to the corresponding argument to use in the Salt program, or 
--   return Nothing which indicates it should be discarded.
convertOrDiscardSuperArgX
        :: Show a                       
        => Exp (AnTEC a E.Name) E.Name  -- ^ Overall application expression, for debugging.
        -> Context
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Maybe (Exp a A.Name))

convertOrDiscardSuperArgX xxApp ctx xx

        -- Region type arguments get passed through directly.
        | XType a t     <- xx
        , isRegionKind (annotType a)
        = do    t'       <- convertRegionT (typeContext ctx) t
                return   $ Just (XType (annotTail a) t')

        -- If we have a data type argument where the type is boxed, then we pass
        -- the region the corresponding Salt object is in.
        | XType a t     <- xx
        , isDataKind   (annotType a)
        = do    let kenv =  contextKindEnv ctx
                t'       <- saltPrimeRegionOfDataType kenv t
                return   $ Just (XType (annotTail a) t')

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
        = do    x'      <- contextConvertExp ctx ExpArg ctx xx
                return  $ Just x'


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
