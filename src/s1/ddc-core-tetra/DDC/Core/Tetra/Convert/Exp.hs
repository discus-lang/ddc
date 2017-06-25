-- | Conversion of Disciple Lite to Disciple Salt.
module DDC.Core.Tetra.Convert.Exp
        (convertExp)
where
import DDC.Core.Tetra.Transform.Curry.Callable
import DDC.Core.Tetra.Convert.Exp.Arg
import DDC.Core.Tetra.Convert.Exp.Ctor
import DDC.Core.Tetra.Convert.Exp.PrimCall
import DDC.Core.Tetra.Convert.Exp.PrimArith
import DDC.Core.Tetra.Convert.Exp.PrimVector
import DDC.Core.Tetra.Convert.Exp.PrimBoxing
import DDC.Core.Tetra.Convert.Exp.PrimRecord
import DDC.Core.Tetra.Convert.Exp.PrimError
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Call                  as Call
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Compounds        as A
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A

import DDC.Type.DataDef
import DDC.Data.Pretty
import Control.Monad
import Data.Maybe
import DDC.Control.Check                        (throw)
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- | Convert the body of a supercombinator to Salt.
convertExp
        :: Show a
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertExp ectx ctx xx
 = let defs         = contextDataDefs    ctx
       convertX     = contextConvertExp  ctx
       convertA     = contextConvertAlt  ctx
       convertLts   = contextConvertLets ctx
       downCtorApp  = convertCtorApp     ctx
   in case xx of

        ---------------------------------------------------
        XVar _ UIx{}
         -> throw $ ErrorUnsupported xx
          $ vcat [ text "Cannot convert program with anonymous value binders."
                 , text "The program must be namified before conversion." ]

        XVar a u
         -> do  let a'  = annotTail a

                u'      <-  convertDataU u
                        >>= maybe (throw $ ErrorInvalidBound u) return

                return  $  XVar a' u'


        ---------------------------------------------------
        -- Ambient primitives.
        XPrim a p
         -> do  let a'  = annotTail a
                return  $ XPrim a' p


        ---------------------------------------------------
        -- Unapplied data constructor.
        XCon a dc
         -> do  xx'     <- downCtorApp a dc []
                return  xx'


        ---------------------------------------------------
        -- Type abstractions can appear in the body of expressions when
        -- the Curry transform has eta-expanded a higher-ranked type.
        XAbs _ (MType b) x
         -> let ctx'    = extendsKindEnv [b] ctx
            in  convertExp ectx ctx' x


        ---------------------------------------------------
        -- Function abstractions can only appear at the top-level of a fucntion.
        XAbs{}
         -> throw $ ErrorUnsupported xx
          $ vcat [ text "Cannot convert function abstraction in this context."
                 , text "The program must be lambda-lifted before conversion." ]


        ---------------------------------------------------
        _
         -- Conversions for primitives in the ambient calculus.
         |  Just p      <- takeNamePrimAppX xx
         ,  Just r
             <- case p of
                  PElaborate{}            -> Nothing
                  PProject{}              -> convertPrimRecord ectx ctx xx
                  PShuffle{}              -> convertPrimRecord ectx ctx xx
                  PCombine{}              -> convertPrimRecord ectx ctx xx
         -> r

         -- Conversions for fragment specific primitive operators.
         |  Just n      <- takeNameFragAppX xx
         ,  Just r
             <- case n of
                  E.NamePrimArith{}       -> convertPrimArith  ectx ctx xx
                  E.NamePrimCast _ True   -> convertPrimArith  ectx ctx xx
                  E.NamePrimCast _ False  -> convertPrimBoxing ectx ctx xx
                  E.NameOpError{}         -> convertPrimError  ectx ctx xx
                  E.NameOpVector{}        -> convertPrimVector ectx ctx xx
                  E.NameOpFun{}           -> convertPrimCall   ectx ctx xx
                  _                       -> Nothing
         -> r

        ---------------------------------------------------
        -- Polymorphic instantiation.
        --  A polymorphic function is being applied without any associated value
        --  arguments. In the Salt code this is a no-op, so just return the
        --  functional value itself. The other cases are handled when converting
        --  let expressions. See [Note: Binding top-level supers]
        --
        XApp _ xa xb
         | (xF, asArgs) <- takeXApps1 xa xb
         , tsArgs       <- [t | RType t <- asArgs]
         , length asArgs == length tsArgs
         , XVar _ (UName n)     <- xF
         , not $ Map.member n (contextCallable ctx)
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
        -- Saturated application of a top-level supercombinator or imported function.
        --  This does not cover application of primops, those are handled by one
        --  of the above cases.
        --
        XApp (AnTEC _t _ _ a') xa xb
         | (x1, asArgs)      <- takeXApps1 xa xb

         -- The thing being applied is a named function that is defined
         -- at top-level, or imported directly.
         , XVar _ (UName nF) <- x1
         , Map.member nF (contextCallable ctx)
         , (tsArgs, _)       <- takeTFunAllArgResult (annotType $ annotOfExp x1)
         -> convertExpSuperCall xx ectx ctx False a' nF
                $ zip asArgs tsArgs

         | otherwise
         -> throw $ ErrorUnsupported xx
         $  vcat [ text "Cannot convert application."
                 , text "fun:       " <> ppr xa
                 , text "args:      " <> ppr xb ]


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
         $  vcat [ text "Cannot convert a let-expression in this context."
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
                tX'     <- convertDataT (typeContext ctx) tX

                tScrut' <- convertDataT (typeContext ctx) tScrut
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

                return  $ XCase a' (A.xTagOfObject a' tPrime x')
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
         $  text "Unsupported case expression form."


        ---------------------------------------------------
        -- Type casts
        -- Run an application of a top-level super.
        XCast _ CastRun (XApp (AnTEC _t _ _ a') xa xb)
         | (xFun, asArgs) <- takeXApps1 xa xb

         -- The thing being applied is a named function that is defined
         -- at top-level, or imported directly.
         , XVar _ (UName nSuper) <- xFun
         , Map.member nSuper (contextCallable ctx)
         , (tsArgs, _)          <- takeTFunAllArgResult (annotType $ annotOfExp xFun)
         -> convertExpSuperCall xx ectx ctx True a' nSuper
                $ zip asArgs tsArgs

        -- Run a suspended computation.
        --   This isn't a super call, so the argument itself will be
        --   represented as a thunk.
        XCast (AnTEC _ _ _ a') CastRun xArg
         -> do
                xArg'   <- convertX ExpArg ctx xArg
                return  $ A.xRunThunk a' A.rTop A.rTop xArg'


        -- Some cast that has no operational behaviour.
        XCast _ _ x
         -> convertX (min ectx ExpBody) ctx x



---------------------------------------------------------------------------------------------------
convertExpSuperCall
        :: Exp (AnTEC a E.Name) E.Name
        -> ExpContext                     -- ^ The surrounding expression context.
        -> Context a                      -- ^ Types and values in the environment.
        -> Bool                           -- ^ Whether this is call is directly inside a 'run'
        ->  a                             -- ^ Annotation from application node.
        ->  E.Name                        -- ^ Name of super.
        -> [ ( Arg (AnTEC a E.Name) E.Name
             , Type E.Name) ]             -- ^ Arguments to super, along with their types.
        -> ConvertM a (Exp a A.Name)

convertExpSuperCall xx _ectx ctx isRun a nFun atsArgs

 -- EITHER Saturated super call where call site is running the result,
 --        and the super itself directly produces a boxed computation.
 --   OR   Saturated super call where the call site is NOT running the result,
 --        and the super itself does NOT directly produce a boxed computation.
 --
 -- In both these cases we can just call the Salt-level super directly.
 --
 | Just (arityVal, boxings)
    <- case Map.lookup nFun (contextCallable ctx) of
        Just (Callable _src _ty cs)
           |  Just (_, csVal, csBox)
                <- Call.splitStdCallCons
                $  filter (not . Call.isConsType) cs

           -> Just (length csVal, length csBox)

        _  -> Nothing

 -- super call is saturated.
 , xsArgsVal        <- filter isRTerm $ map fst atsArgs
 , length xsArgsVal == arityVal

 -- no run/box to get in the way.
 ,   ( isRun      && boxings == 1)
  || ((not isRun) && boxings == 0)
 = do
        -- Convert the functional part.
        uF      <-  convertDataU (UName nFun)
                >>= maybe (throw $ ErrorInvalidBound (UName nFun)) return

        -- Convert the arguments.
        -- Effect type and witness arguments are discarded here.
        xsArgs' <- liftM catMaybes
                $  mapM (convertOrDiscardSuperArgX ctx)
                        atsArgs

        return  $ xApps a (XVar a uF) xsArgs'


 -- We can't make the call,
 -- so emit some debugging info.
 | otherwise
 = throw $ ErrorUnsupported xx
 $ vcat [ text "Cannot convert application for super call."
        , text "xx:        " <> ppr xx
        , text "fun:       " <> ppr nFun
        , text "args:      " <> ppr atsArgs
        ]


---------------------------------------------------------------------------------------------------
-- | If this is an application of a fragment specific primitive or
--   the result of running one then take its name.
takeNameFragAppX :: Exp a E.Name -> Maybe E.Name
takeNameFragAppX xx
 = case xx of
        XApp{}
          -> case takeXFragApps xx of
                Just (n, _)     -> Just n
                Nothing         -> Nothing

        XCast _ CastRun xx'@XApp{}
          -> takeNameFragAppX xx'

        _ -> Nothing


-- | If this is an application of an ambient primitive,
--   then return its name.
takeNamePrimAppX :: Exp a E.Name -> Maybe Prim
takeNamePrimAppX xx
 = case xx of
        XApp{}
          -> case takeXPrimApps xx of
                Just (p, _)     -> Just p
                Nothing         -> Nothing

        _ -> Nothing
