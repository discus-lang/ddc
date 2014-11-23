-- | Conversion of Disciple Lite to Disciple Salt.
module DDC.Core.Tetra.Convert.Exp
        (convertExp)
where
import DDC.Core.Tetra.Convert.Exp.Arg
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
        -> Context a                    -- ^ Types and values in the environment.
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
        -- Polymorphic instantiation.
        --  A polymorphic function is being applied without any associated type
        --  arguments. In the Salt code this is a no-op, so just return the 
        --  functional value itself. The other cases are handled when converting
        --  let expressions. See [Note: Binding top-level supers]
        --
        XApp _ xa xb
         | (xF, xsArgs) <- takeXApps1 xa xb
         , tsArgs       <- [t | XType _ t <- xsArgs]
         , length xsArgs == length tsArgs
         , XVar _ (UName n)     <- xF
         , not $ Set.member n (contextSupers  ctx)
         , not $ Set.member n (contextImports ctx)      -- TODO: can bind vals wit arity == 0
                                                        --       but not others.
         -> convertX ExpBody ctx xF

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


        ---------------------------------------------------
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
                (mlts', ctx')   <- convertLts ctx lts

                -- Convert the body of the expression.
                x2'             <- convertX ExpBody ctx' x2

                case mlts' of
                 Nothing        -> return $ x2'
                 Just lts'      -> return $ XLet (annotTail a) lts' x2'

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

