{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.Base
        ( Checker
        , Demand (..)
        , Table  (..)
        , returnX
        , runForDemand
        , isHoleT
        , checkTypeM
        , checkBindM

        , module DDC.Core.Check.Judge.Inst
        , module DDC.Core.Check.Judge.Sub
        , module DDC.Core.Check.Judge.EqT
        , module DDC.Core.Check.Judge.Witness
        , module DDC.Core.Check.Error
        , module DDC.Core.Check.Base

        , module DDC.Core.Transform.Reannotate
        , module DDC.Core.Transform.SubstituteTX
        , module DDC.Core.Exp.Annot.AnTEC

        , module DDC.Type.Transform.SubstituteT
        , module DDC.Type.Transform.Instantiate
        , module DDC.Type.Transform.BoundT)
where
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Judge.Sub
import DDC.Core.Check.Judge.EqT
import DDC.Core.Check.Judge.Witness
import DDC.Core.Check.Error
import DDC.Core.Check.Base

import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Exp.Annot.AnTEC

import DDC.Core.Check.Judge.Kind
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.BoundT


-- | Demand placed on suspensions by the surrounding context.
data Demand
        -- | Run suspensions as we encounter them.
        = DemandRun

        -- | Ignore suspensions, don't run them.
        | DemandNone
        deriving Show

instance Pretty Demand where
 ppr DemandRun  = text "Run"
 ppr DemandNone = text "None"


-- | Type of the function that checks some node of the core AST.
type Checker a n
        =  (Show a, Show n, Ord n, Pretty n)
        => Table a n                    -- ^ Static configuration.
        -> Context n                    -- ^ Input context.
        -> Mode n                       -- ^ Type checker mode.
        -> Demand                       -- ^ Demand on the expression.
        -> Exp a n                      -- ^ Expression to check.
        -> CheckM a n
                ( Exp (AnTEC a n) n     -- Annotated, checked expression.
                , Type n                -- Type of the expression.
                , TypeSum n             -- Effect sum of expression.
                , Context n)            -- Output context.


-- | Table of environment things that do not change during type checking
--
--   We've got the static config,
--    global kind and type environments,
--    and a type checking function for each node of the AST.
--
--   We split the type checker into separate functions and dispatch them
--   via this table so we can handle each AST node in a separate module,
--   while avoiding the explicit mutual recursion. If the functions were
--   explicitly mutually recursive then we'd need to write GHC boot modules,
--   which is annoying.
--
data Table a n
        = Table
        { tableConfig           :: Config n
        , tableCheckExp         :: Checker a n
        , tableCheckPrim        :: Checker a n
        , tableCheckVarCon      :: Checker a n
        , tableCheckAppT        :: Checker a n
        , tableCheckAppX        :: Checker a n
        , tableCheckLamT        :: Checker a n
        , tableCheckLamX        :: Checker a n
        , tableCheckLet         :: Checker a n
        , tableCheckLetPrivate  :: Checker a n
        , tableCheckCase        :: Checker a n
        , tableCheckCast        :: Checker a n }


-- | Helper function for building the return value of checkExpM'
--   It builts the AnTEC annotation and attaches it to the new AST node,
--   as well as returning the current effect and closure in the appropriate
--   form as part of the tuple.
returnX :: Ord n
        => a                            -- ^ Annotation for the returned expression.
        -> (AnTEC a n
                -> Exp (AnTEC a n) n)   -- ^ Fn to build the returned expression.
        -> Type n                       -- ^ Type of expression.
        -> TypeSum n                    -- ^ Effect sum of expression.
        -> Context n                    -- ^ Input context.
        -> CheckM a n
                ( Exp (AnTEC a n) n     -- Annotated, checked expression.
                , Type n                -- Type of expression.       (id to above)
                , TypeSum n             -- Effect sum of expression. (id to above)
                , Context n)            -- Output context.

returnX !a !f !t !es !ctx
 = let  e       = TSum es
   in   return  (f (AnTEC t e (tBot kClosure) a)
                , t, es, ctx)
{-# INLINE returnX #-}


-- Run ------------------------------------------------------------------------
-- | If an expression has suspension type then run it.
runForDemand
        :: Ord n
        => Config n                     -- ^ Type checker config.
        -> a                            -- ^ Annotation for new
        -> Demand                       -- ^ Demand placed on expression.
        -> Exp    (AnTEC a n) n         -- ^ Expression to inspect.
        -> Type   n                     -- ^ Type of the expression.
        -> Effect n                     -- ^ Effect of the expression.
        -> CheckM a n
                ( Exp (AnTEC a n) n     -- New expression, possibly with run cast.
                , Type   n              -- New type of expression.
                , Effect n)             -- New effect of expression.

runForDemand _config _a DemandNone xExp tExp eExp
 = return (xExp, tExp, eExp)

runForDemand config a  DemandRun  xExp tExp eExp

 -- If the expression is wrapped in an explicit box or run then
 -- don't run it again. Doing this will just confuse the client
 -- programmer.
 | isXCastBox xExp || isXCastRun xExp
 = return (xExp, tExp, eExp)

 -- Insert an implicit run cast for this suspension.
 | configImplicitRun config
 , Just (eResult, tResult)  <- takeTSusp tExp
 = let
        -- Effect of overall expression is effect of computing
        -- the suspension plus the effect we get by running
        -- that suspension.
        eTotal  = tSum kEffect [eExp, eResult]

        -- Annotation for the cast expression.
        aCast   = AnTEC tResult eTotal (tBot kClosure) a

   in   return  ( XCast aCast CastRun xExp
                , tResult
                , eTotal)

 | otherwise
 = return (xExp, tExp, eExp)


-------------------------------------------------------------------------------
isHoleT :: Config n -> Type n -> Bool
isHoleT config tt
 = case tt of
        TVar u
         -> case u of
                UName n
                 -> case configNameIsHole config of
                        Nothing -> False
                        Just f  -> f n
                _       -> False

        _ -> isBot tt


-- | Check the type of a bind.
checkBindM
        :: (Ord n, Show n, Pretty n)
        => Config n     -- ^ Checker configuration.
        -> Context n    -- ^ Type context.
        -> Universe     -- ^ Universe for the type of the bind.
        -> Bind n       -- ^ Check this bind.
        -> Mode n       -- ^ Mode for bidirectional checking.
        -> CheckM a n (Bind n, Type n, Context n)

checkBindM config ctx uni bb mode
 = do   (t', k, ctx')  <- checkTypeM config ctx uni (typeOfBind bb) mode
        return (replaceTypeOfBind t' bb, k, ctx')

