
-- | Call patterns.
--
--   A call pattern describes the sequence of objects that are eliminated
--   by some object when we apply it, and before it starts constructing
--   new values. 
--
--   Constructor (+ve)             Eliminator (-ve)
--    /\x.  (type   abstraction)    @'    (type   application)
--     \x.  (object abstraction)    @     (object application) 
--    box   (suspend evaluation)    run   (commence evaluation)
--    
module DDC.Core.Call 
        ( -- * Call constructors
          Cons (..)
        , isConsType
        , isConsValue
        , isConsBox
        , takeCallConsFromExp
        , takeCallConsFromType
        , splitStdCallCons
        , takeStdCallConsFromTypeArity

          -- * Call eliminators
        , Elim (..)
        , isElimType
        , isElimValue
        , isElimRun
        , takeCallElim
        , applyElim
        , splitStdCallElims

          -- * Matching
        , elimForCons
        , dischargeConsWithElims
        , dischargeTypeWithElims)
where
import DDC.Core.Exp.Annot
import DDC.Type.Transform.SubstituteT


-----------------------------------------------------------------------------
-- | One component of the call pattern of a super.
--   This is the "outer wrapper" of the computation,
-- 
--   Eg, with  /\(a : k). \(x : t). box (x + 1) the call pattern consists of
--   the two lambdas and the box. These three things need to be eliminated
--   before we can construct any new values.
--
data Cons n
        = -- | A type  lambda that needs a type of this kind.
          ConsType    (Bind n)

          -- | A value lambda that needs a value of this type.
        | ConsValue   (Type n)

          -- | A suspended expression that needs to be run.
        | ConsBox
        deriving (Show)


-- | Check if this is an `ConsType`.
isConsType :: Cons n -> Bool
isConsType cc
 = case cc of
        ConsType{}      -> True
        _               -> False


-- | Check if this is an `ElimType`.
isConsValue :: Cons n -> Bool
isConsValue cc
 = case cc of
        ConsValue{}     -> True
        _               -> False


-- | Check if this is an `ElimType`.
isConsBox :: Cons n -> Bool
isConsBox cc
 = case cc of
        ConsBox{}       -> True
        _               -> False


-- | Get the call pattern of an expression.
takeCallConsFromExp :: Exp a n -> [Cons n]
takeCallConsFromExp xx
 = case xx of
        XLAM _ b x         
         ->     ConsType  b : takeCallConsFromExp x

        XLam _ b x         
         -> let t       = typeOfBind b
            in  ConsValue t : takeCallConsFromExp x

        XCast _ CastBox x
         ->     ConsBox     : takeCallConsFromExp x

        _ -> []


-- | Infer the call pattern of an expression from its type.
--   If the type has a function constructor then we assume there
--   is a corresponding lambda abstraction in the expression, and so on.
takeCallConsFromType :: Type n -> [Cons n]
takeCallConsFromType tt
        | TForall bParam tBody   <- tt
        = ConsType  bParam : takeCallConsFromType tBody

        | Just (tParam, tResult) <- takeTFun tt
        = ConsValue tParam : takeCallConsFromType tResult

        | Just (_, tResult)      <- takeTSusp tt
        = ConsBox          : takeCallConsFromType tResult

        | otherwise
        = []


-- | Like `splitStdCallElim`, but for the constructor side.
--
splitStdCallCons
        :: [Cons n]
        -> Maybe ([Cons n], [Cons n], [Cons n])

splitStdCallCons cs
 = eatTypes [] cs
 where
        eatTypes  accTs (e@ConsType{} : es)
         = eatTypes (e : accTs) es

        eatTypes  accTs es
         = eatValues (reverse accTs) [] es

        eatValues accTs accVs (e@ConsValue{} : es)
         = eatValues accTs (e : accVs) es

        eatValues accTs accVs es
         = eatRuns   accTs (reverse accVs) [] es

        eatRuns  accTs accVs accRs (e@ConsBox{} : es)
         = eatRuns   accTs accVs (e : accRs) es

        eatRuns  accTs accVs accRs []
         = Just (accTs, accVs, reverse accRs)

        eatRuns  _accTs _accVs _accRs _
         = Nothing


-- | Given the type of a super, and the number of type parameters,
--   value parameters and boxings, produce the corresponding list
--   of call constructors.
--
--   Example:
--    takeStdCallConsFromType 
--       [| forall (a : k1) (b : k2). a -> b -> S e b |] 
--       2 2 1
--
--    => [ ConsType  [|k1|], ConsType  [|k2|]
--       , ConsValue [|a\],  ConsValue [|b|]
--       , ConsBox ]
--
--   When we're considering the parts of the type, if the given arity
--   does not match what is in the type then `Nothing`.
--
takeStdCallConsFromTypeArity
        :: Type n       -- ^ Type of super
        -> Int          -- ^ Number of type parameters.
        -> Int          -- ^ Number of value parameters.
        -> Int          -- ^ Number of boxings.
        -> Maybe [Cons n]

takeStdCallConsFromTypeArity tt0 nTypes0 nValues0 nBoxes0
 = eatTypes [] tt0 nTypes0
 where
        -- Consider type parameters.
        eatTypes !accTs !tt !nTypes

         -- The arity information tells us to expect a type parameter.
         | nTypes  > 0
         = case tt of
            -- The super type matches.
            TForall b tBody
             -> eatTypes (ConsType b : accTs) tBody (nTypes - 1)

            -- The super type does not match the arity information.
            _ -> Nothing

         -- No more type parameters expected, so consider the value parameters.
         | otherwise
         = eatValues (reverse accTs) [] tt nValues0


        -- Consider value parameters.
        eatValues !accTs !accVs !tt !nValues

         -- The arity information tells us to expect a value parameter.
         | nValues > 0
         = case takeTFun tt of
            -- The super type matches.
            Just (t1, t2) 
              -> eatValues accTs (ConsValue t1 : accVs) t2 (nValues - 1)

            -- The super type does not match the arity information.
            _ -> Nothing

         -- No more value parameters expect, so consider the boxes.
         | otherwise
         = eatBoxes accTs (reverse accVs) [] tt nBoxes0


        -- Consider boxes.
        eatBoxes !accTs !accVs !accBs tt nBoxes

         -- The arity information tells us to expect a boxing.
         | nBoxes > 0
         = case takeTSusp tt of
            -- The super type matches.
            Just (_eff, tBody)
              -> eatBoxes accTs accVs (ConsBox : accBs) tBody (nBoxes - 1)

            -- The super type does not match the arity information.
            _ -> Nothing

         -- No more boxings to expect, so we're done.
         | otherwise
         = return (accTs ++ accVs ++ reverse accBs)


-------------------------------------------------------------------------------
-- | One component of a super call.
data Elim a n
        = -- | Give a type to a type lambda.
          ElimType    a a (Type n)

          -- | Give a value to a value lambda.
        | ElimValue   a (Exp a n)

          -- | Run a suspended computation.
        | ElimRun     a
        deriving (Show)


-- | Check if this is an `ElimType`.
isElimType :: Elim a n -> Bool
isElimType ee
 = case ee of
        ElimType{}      -> True
        _               -> False


-- | Check if this is an `ElimType`.
isElimValue :: Elim a n -> Bool
isElimValue ee
 = case ee of
        ElimValue{}     -> True
        _               -> False


-- | Check if this is an `ElimType`.
isElimRun :: Elim a n -> Bool
isElimRun ee
 = case ee of
        ElimRun{}       -> True
        _               -> False


-- | Apply an eliminator to an expression.
applyElim :: Exp a n -> Elim a n -> Exp a n
applyElim xx e
 = case e of
        ElimType  a at t -> XApp a xx (XType at t)
        ElimValue a x    -> XApp a xx x
        ElimRun   a      -> XCast a CastRun xx


-- | Split the application of some object into the object being
--   applied and its eliminators.
takeCallElim :: Exp a n -> (Exp a n, [Elim a n])
takeCallElim xx
 = case xx of
        XApp a x1 (XType at t2)
         -> let (xF, xArgs)     = takeCallElim x1
            in  (xF, xArgs ++ [ElimType a at t2])

        XApp a x1 x2            
         -> let (xF, xArgs)     = takeCallElim x1
            in  (xF, xArgs ++ [ElimValue a x2])

        XCast a CastRun x1
         -> let (xF, xArgs)     = takeCallElim x1
            in  (xF, xArgs ++ [ElimRun a])

        _ -> (xx, [])


-- | Group eliminators into sets for a standard call.
--
--   The standard call sequence is a list of type arguments, followed
--   by some objects, and optionally running the result suspension.
--
--   @run f [T1] [T2] x1 x2@
--
--   If 'f' is a super, and this is a saturating call then the super header
--   will look like the following:
--
--   @f = (/\t1. /\t2. \v1. \v2. box. body)@

--   If the eliminators are not in the standard call sequence then `Nothing`.
--
splitStdCallElims 
        :: [Elim a n] 
        -> Maybe ([Elim a n], [Elim a n], [Elim a n])

splitStdCallElims ee
 = eatTypes [] ee
 where
        eatTypes  accTs (e@ElimType{} : es)
         = eatTypes (e : accTs) es

        eatTypes  accTs es
         = eatValues (reverse accTs) [] es

        eatValues accTs accVs (e@ElimValue{} : es)
         = eatValues accTs (e : accVs) es

        eatValues accTs accVs es
         = eatRuns   accTs (reverse accVs) [] es

        eatRuns  accTs accVs accRs (e@ElimRun{} : es)
         = eatRuns   accTs accVs (e : accRs) es

        eatRuns  accTs accVs accRs []
         = Just (accTs, accVs, reverse accRs)

        eatRuns  _accTs _accVs _accRs _
         = Nothing


-------------------------------------------------------------------------------
-- | Check if this an eliminator for the given constructor.
--   This only checks the general form of the eliminator 
--   and constructor, not the exact types or kinds.
elimForCons :: Elim a n -> Cons n -> Bool
elimForCons e c
 = case (e, c) of
        (ElimType{},  ConsType{})       -> True
        (ElimValue{}, ConsValue{})      -> True
        (ElimRun{},   ConsBox{})        -> True
        _                               -> False


-- | Given lists of constructors and eliminators, check if the
--   eliminators satisfy the constructors, and return any remaining
--   unmatching constructors and eliminators.
--
--   We assume that the application is well typed and that applying
--   the given eliminators will not cause variable capture.
---
--   ISSUE #347: Avoid name capture in dischargeConsWithElims
--   This process doesn't avoid name capture by ConsTypes earlier
--   in the list, but it's only called from the Curry transform
--   where there shouldn't be any shadowed type binders.
--
dischargeConsWithElims
        :: Ord n
        => [Cons n] 
        -> [Elim a n] 
        -> ([Cons n], [Elim a n])

dischargeConsWithElims (c : cs) (e : es)
 = case (c, e) of
        (ConsType  b1, ElimType  _ _ t2)
          -> dischargeConsWithElims 
                (map (instantiateConsT b1 t2) cs) 
                es

        (ConsValue _t1, ElimValue _ _x2)
          -> dischargeConsWithElims cs es

        (ConsBox,       ElimRun _)
          -> dischargeConsWithElims cs es

        _ -> (c : cs, e : es)

dischargeConsWithElims cs es
 = (cs, es)


instantiateConsT :: Ord n => Bind n -> Type n -> Cons n -> Cons n
instantiateConsT b t cc
 = case cc of
        ConsType{}      -> cc
        ConsValue t'    -> ConsValue (substituteT b t t')
        ConsBox{}       -> cc


-- | Given a type of a function and eliminators, discharge
--   foralls, abstractions and boxes to get the result type
--   of performing the application.
-- 
--   We assume that the application is well typed.
--
dischargeTypeWithElims
        :: Ord n
        => Type n
        -> [Elim a n]
        -> Maybe (Type n)

dischargeTypeWithElims tt (ElimType  _ _ tArg : es)
        | TForall b tBody         <- tt
        = dischargeTypeWithElims 
                (substituteT b tArg tBody) 
                es

dischargeTypeWithElims tt (ElimValue _ _xArg  : es)
        | Just (_tParam, tResult) <- takeTFun tt
        = dischargeTypeWithElims tResult es

dischargeTypeWithElims tt (ElimRun _ : es)
        | Just (_, tBody)         <- takeTSusp tt
        = dischargeTypeWithElims tBody es
 
dischargeTypeWithElims tt []
        = Just tt

dischargeTypeWithElims _tt _es
        = Nothing

