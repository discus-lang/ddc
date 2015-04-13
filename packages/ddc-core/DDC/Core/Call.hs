
-- | Call patterns.
--
--   Abstractly, a call pattern describes the sequence of objects that is
--   eliminated by some procedure when we call it, and before it starts
--   constructing new values. This definition includes any use of 'box',
--   as the 'box' construct acts like a lambda expression that suspends
--   computation but does not bind a value. 
--
--   Constructor (+ve)            Eliminator (-ve)
--    /\x.  (type  lambda)         @'    (type  lambda)
--     \x.  (value lambda)         @     (value application)
--    box   (suspend evaluation)   run   (commence evaluation)
--    
--   TODO: if we changed case to \case we could also have.
--    C     (algebraic data)       \case (case match)

module DDC.Core.Call 
        ( Cons (..)

        , Elim (..)
        , isElimType
        , isElimValue
        , isElimRun

        , takeCallCons
        , takeCallElim

        , elimForCons
        , applyElim)
where
import DDC.Core.Exp
import DDC.Core.Compounds


-- | One component of the call pattern of a super.
--   This is the "outer wrapper" of the computation,
-- 
--   Eg, with  /\(a : k). \(x : t). box (x + 1) the call pattern consists of
--   the two lambdas and the box. These three things need to be eliminated
--   before we can construct any new values.
--
data Cons n
        = -- | A type  lambda that needs a type of this kind.
          ConsType    (Kind n)

          -- | A value lambda that needs a value of this type.
        | ConsValue   (Type n)

          -- | A suspended expression that needs to be run.
        | ConsBox
        deriving (Show)


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


-- | Get the call pattern of an object.
takeCallCons :: Exp a n -> [Cons n]
takeCallCons xx
 = case xx of
        XLAM _ b x         -> ConsType  (typeOfBind b) : takeCallCons x
        XLam _ b x         -> ConsValue (typeOfBind b) : takeCallCons x
        XCast _ CastBox x  -> ConsBox                  : takeCallCons x
        _                  -> []


-- | Split the application of some object into the object being
--   applied and the values passed to its eliminators.
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


applyElim :: Exp a n -> Elim a n -> Exp a n
applyElim xx e
 = case e of
        ElimType  a at t -> XApp a xx (XType at t)
        ElimValue a x    -> XApp a xx x
        ElimRun   a      -> XCast a CastRun xx


