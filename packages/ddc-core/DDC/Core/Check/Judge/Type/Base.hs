
module DDC.Core.Check.Judge.Type.Base 
        ( Checker
        , Table (..)
        , returnX

        -- * Modules
        , module DDC.Core.Check.Base

        , module DDC.Core.Check.Judge.Inst
        , module DDC.Core.Check.Judge.Sub
        , module DDC.Core.Check.Judge.Eq
        , module DDC.Core.Check.TaggedClosure
        , module DDC.Core.Check.Witness
        , module DDC.Core.Check.Error
        , module DDC.Core.Transform.Reannotate
        , module DDC.Core.Transform.SubstituteTX
        , module DDC.Core.Annot.AnTEC

        , module DDC.Type.Transform.SubstituteT
        , module DDC.Type.Transform.Instantiate
        , module DDC.Type.Transform.Crush
        , module DDC.Type.Transform.LiftT
        , module DDC.Type.Transform.Trim)
where
import DDC.Core.Check.Base

import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Judge.Sub
import DDC.Core.Check.Judge.Eq
import DDC.Core.Check.TaggedClosure
import DDC.Core.Check.Witness
import DDC.Core.Check.Error
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Annot.AnTEC

import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.Crush
import DDC.Type.Transform.LiftT
import DDC.Type.Transform.Trim


-- | Type of the function that checks some node of the core AST.
type Checker a n
        =  (Show n, Ord n, Pretty n)
        => Table a n
        -> Context n
        -> Exp a n      
        -> Mode n
        -> CheckM a n
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n)
                , Context n)


-- | Table of environment things that do not change during type checking
--   We've got the static config, 
--    global kind and type environments,
--    and a type checking function for each node of the AST.
data Table a n
        = Table
        { tableConfig           :: Config n
        , tableKindEnv          :: KindEnv n
        , tableTypeEnv          :: TypeEnv n
        , tableCheckExp         :: Checker a n
        , tableCheckVarCon      :: Checker a n
        , tableCheckApp         :: Checker a n
        , tableCheckLamT        :: Checker a n
        , tableCheckLamX        :: Checker a n
        , tableCheckLet         :: Checker a n
        , tableCheckCase        :: Checker a n
        , tableCheckCast        :: Checker a n 
        , tableCheckWitness     :: Checker a n }


-- | Helper function for building the return value of checkExpM'
--   It builts the AnTEC annotation and attaches it to the new AST node,
--   as well as returning the current effect and closure in the appropriate
--   form as part of the tuple.
returnX :: Ord n 
        => a
        -> (AnTEC a n -> Exp (AnTEC a n) n)
        -> Type n 
        -> TypeSum n
        -> Set (TaggedClosure n)
        -> Context n
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n)
                , Context n)

returnX !a !f !t !es !cs !ctx
 = let  e       = TSum es
        c       = closureOfTaggedSet cs
   in   return  (f (AnTEC t e c a)
                , t, es, cs, ctx)
{-# INLINE returnX #-}

