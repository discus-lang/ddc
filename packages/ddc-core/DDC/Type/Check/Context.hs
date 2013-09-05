
module DDC.Type.Check.Context
        ( Direction     (..)
        , Elem          (..)
        , Context       (..)
        , emptyContext
        , pushType, pushTypes, lookupType, memberType
        , pushKind, pushKinds, lookupKind, memberKind, memberKindBind
        , popToPos
        , liftTypes)
where
import DDC.Type.Exp
import DDC.Type.Transform.LiftT
import Data.Maybe


-- | Direction used for bidirectional type checking.
data Direction n
        -- | Check the type of an expression against this one.
        = Check (Type n)

        -- | Synthesise the type of the expression.
        | Synth
        deriving Show


-- | An element in the type checker context.
data Elem n
        = ElemKind (Bind n)
        | ElemType (Bind n)
        deriving Show

-- | The type checker context.
data Context n
        = Context !Int [Elem n]
        deriving Show


-- | A position in the type checker context.
data Pos
        = Pos Int


-- | An empty context.
emptyContext :: Context n
emptyContext 
        = Context 0 []


-- Push -----------------------------------------------------------------------
-- | Push the type of some value variable onto the context.
pushType  :: Bind n -> Context n -> (Context n, Pos)
pushType b (Context len ls)
 =      ( Context (len + 1) (ElemType b : ls)
        , Pos len)


-- | Push many types onto the context.
pushTypes :: [Bind n] -> Context n -> (Context n, Pos)
pushTypes bs (Context len ls)
 =      ( Context (len + length bs) 
                 ( reverse [ ElemType b | b <- bs] ++ ls)
        , Pos len)


-- | Push the kind of some type variable onto the context.
pushKind :: Bind n -> Context n -> (Context n, Pos)
pushKind b (Context len ls)
 =      ( Context (len + 1) (ElemKind b : ls)
        , Pos len)


-- | Push many kinds onto the context.
pushKinds :: [Bind n] -> Context n -> (Context n, Pos)
pushKinds bs (Context len ls)
 =      ( Context (len + length bs) 
                 ( reverse [ ElemKind b | b <- bs] ++ ls)
        , Pos len)


-- Pop ------------------------------------------------------------------------
-- | Pop elements from a context to get back to the given position.
popToPos :: Pos -> Context n -> Maybe (Context n)
popToPos (Pos pos) (Context len ll)
 = cut len ll
 where
        cut 0    []     = Just $ Context 0 []
        cut _    []     = Nothing
        cut len' (l:ls)
         | len' > pos   = cut (len - 1) ls
         | otherwise    = Just $ Context pos (l:ls)


-- Lookup ---------------------------------------------------------------------
-- | Lookup the type of some variable from the context.
lookupType :: Eq n => Bound n -> Context n -> Maybe (Type n)
lookupType u (Context _ ll)
 = case u of
        UPrim{}         -> Nothing
        UName n         -> goName n    ll
        UIx   ix        -> goIx   ix 0 ll
 where
        goName _n []    = Nothing
        goName n  (ElemType (BName n' t) : ls)
         | n == n'      = Just t
         | otherwise    = goName n ls
        goName  n (_ : ls)
         = goName n ls


        goIx _ix _d []  = Nothing
        goIx ix d  (ElemType (BAnon t) : ls)
         | ix == d      = Just t
         | otherwise    = goIx   ix (d + 1) ls
        goIx ix d  (_ : ls)
         = goIx ix d ls


-- | See if this type variable is in the context.
memberType :: Eq n => Bound n -> Context n -> Bool
memberType u ctx = isJust $ lookupType u ctx


-- | Lookup the kind of some variable from the context.
lookupKind :: Eq n => Bound n -> Context n -> Maybe (Kind n)
lookupKind u (Context _ ll)
 = case u of
        UPrim{}         -> Nothing
        UName n         -> goName n    ll
        UIx   ix        -> goIx   ix 0 ll
 where
        goName _n []    = Nothing
        goName n  (ElemKind (BName n' t) : ls)
         | n == n'      = Just t
         | otherwise    = goName n ls
        goName  n (_ : ls)
         = goName n ls


        goIx _ix _d []  = Nothing
        goIx ix d  (ElemKind (BAnon t) : ls)
         | ix == d      = Just t
         | otherwise    = goIx   ix (d + 1) ls
        goIx ix d  (_ : ls)
         = goIx ix d ls


-- | See if this kind variable is in the context.
memberKind :: Eq n => Bound n -> Context n -> Bool
memberKind u ctx = isJust $ lookupKind u ctx


-- | See if the name on a named binder is in the contexts.
--   Returns False for non-named binders.
memberKindBind :: Eq n => Bind n -> Context n -> Bool
memberKindBind b ctx
 = case b of
        BName n _       -> memberKind (UName n) ctx
        _               -> False


-- Lifting --------------------------------------------------------------------
-- | Lift free debruijn indices in types by the given number of levels.
liftTypes :: Ord n => Int -> Context n -> Context n
liftTypes n (Context len ll)
 = Context len (go ll)
 where
        go []                   = []
        go (ElemType b : ls)    = ElemType (liftT n b) : go ls
        go (l:ls)               = l : go ls

