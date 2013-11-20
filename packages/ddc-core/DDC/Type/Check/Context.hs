
module DDC.Type.Check.Context
        ( Mode    (..)

        -- * Existentials
        , Exists  (..)
        , typeOfExists
        , takeExists

        , Elem    (..)
        , Role    (..)
        , Context (..)
        , emptyContext

        -- Positions
        , Pos     (..)
        , markContext
        , popToPos

        -- Pushing
        , pushType,   pushTypes, memberType
        , pushKind,   pushKinds, memberKind, memberKindBind
        , pushExists

        -- Lookup
        , lookupType
        , lookupKind
        , lookupExistsEq

        -- Existentials
        , locationOfExists
        , updateExists

        , applyContext
        , effectSupported

        , liftTypes
        , lowerTypes)
where
import DDC.Type.Exp
import DDC.Type.Pretty
import DDC.Type.Transform.LiftT
import DDC.Type.Compounds
import DDC.Base.Pretty                  ()
import Data.Maybe
import qualified DDC.Type.Sum           as Sum


-- Mode -----------------------------------------------------------------------
-- | What mode we're performing type checking/inference in.
data Mode n
        -- | Reconstruct the type of the expression, requiring type annotations
        --   on parameters  as well as type applications to already be present.
        = Recon
        
        -- | Synthesise the type of the expression, producing unification
        ---  variables for bidirectional type inference.
        | Synth

        -- | heck the type of an expression against this expected type, and
        --   unify expected types into unification variables for bidirecional
        --   type inference.
        | Check (Type n)
        deriving Show


instance (Eq n, Pretty n) => Pretty (Mode n) where
 ppr mode
  = case mode of
        Recon   -> text "RECON"
        Synth   -> text "SYNTH"
        Check t -> text "CHECK" <+> parens (ppr t)


-- Exists ---------------------------------------------------------------------
-- | An existential variable.
data Exists
        = Exists !Int
        deriving (Show, Eq)


instance Pretty Exists where
 ppr (Exists i) = text "?" <> ppr i


-- | Wrap an existential variable into a type.
typeOfExists :: Exists -> Type n
typeOfExists (Exists n)
        = TCon (TyConExists n kData)


-- | Take an Exists from a type.
takeExists :: Type n -> Maybe Exists
takeExists tt
 = case tt of
        TCon (TyConExists n _)  -> Just (Exists n)
        _                       -> Nothing


-- Context --------------------------------------------------------------------
-- | The type checker context.
-- 
--   Holds a position counter and a stack of elements. 
--   The top of the stack is at the front of the list.
--
data Context n
        = Context 
        { contextGenPos         :: !Int
        , contextGenExists      :: !Int 
        , contextElems          :: ![Elem n] }
        deriving Show


instance (Pretty n, Eq n) => Pretty (Context n) where
 ppr (Context genPos genExists ls)
  =   text "Context "
  <$> text "  genPos    = " <> int genPos
  <$> text "  genExists = " <> int genExists
  <$> indent 2 
        (vcat $ [int i <> (indent 4 $ ppr l)
                        | l <- reverse ls
                        | i <- [0..]])


-- Positions -------------------------------------------------------------------
-- | A position in the type checker context.
--   A position is used to record a particular position in the context stack,
--   so that we can pop elements higher than it.
data Pos
        = Pos !Int
        deriving (Show, Eq)

instance Pretty Pos where
 ppr (Pos p)
        = text "*" <> int p


-- Elem -----------------------------------------------------------------------
-- | An element in the type checker context.
data Elem n
        -- | A context position marker.
        = ElemPos       !Pos

        -- | Kind of some variable.
        | ElemKind      !(Bind n) !Role

        -- | Type of some variable.
        | ElemType      !(Bind n)

        -- | Existential variable declaration
        | ElemExistsDecl !Exists

        -- | Existential variable solved to some monotype.
        | ElemExistsEq   !Exists !(Type n)
        deriving (Show, Eq)


-- | The role of some type variable.
data Role
        -- | Concrete type variables are region variables that have been introduced
        --   in an enclosing lexical scope. All the capabilities for these will 
        --   also be in the context.
        = RoleConcrete
        
        -- | Abstract type variables are the ones that are bound by type abstraction
        --   Inside the body of a type abstraction we can assume a region supports
        --   any given capability. We only need to worry about if it really does
        --   when we actually run the enclosed computation.
        | RoleAbstract
        deriving (Show, Eq)


instance (Pretty n, Eq n) => Pretty (Elem n) where
 ppr ll
  = case ll of
        ElemPos p
         -> ppr p

        ElemKind b role  
         -> ppr (binderOfBind b) 
                <+> text "::" 
                <+> (ppr $ typeOfBind b)
                <+> text "@" <> ppr role

        ElemType b
         -> ppr (binderOfBind b)
                <+> text "::"
                <+> (ppr $ typeOfBind b)

        ElemExistsDecl i
         -> ppr i

        ElemExistsEq i t 
         -> ppr i <+> text "=" <+> ppr t


instance Pretty Role where
 ppr role
  = case role of
        RoleConcrete    -> text "Concrete"
        RoleAbstract    -> text "Abstract"


-- Empty ----------------------------------------------------------------------
-- | An empty context.
emptyContext :: Context n
emptyContext 
        = Context 0 0 []


-- Push -----------------------------------------------------------------------
-- | Push the type of some value variable onto the context.
pushType  :: Bind n -> Context n -> Context n
pushType b ctx
 = ctx { contextElems = ElemType b : contextElems ctx }


-- | Push many types onto the context.
pushTypes :: [Bind n] -> Context n -> Context n
pushTypes bs ctx
 = foldl (flip pushType) ctx bs


-- | Push the kind of some type variable onto the context.
pushKind :: Bind n -> Role -> Context n -> Context n
pushKind b role ctx
 = ctx { contextElems = ElemKind b role : contextElems ctx }


-- | Push many kinds onto the context.
pushKinds :: [(Bind n, Role)] -> Context n -> Context n
pushKinds brs ctx
 = foldl (\ctx' (b, r) -> pushKind b r ctx') ctx brs

-- | Push an existential declaration onto the context.
--   If this is not an existential then `error`.
pushExists :: Exists -> Context n -> Context n
pushExists i ctx
 = ctx { contextElems = ElemExistsDecl i : contextElems ctx }


-- Mark / Pop -----------------------------------------------------------------
-- | Mark the context with a new position.
markContext :: Context n -> (Context n, Pos)
markContext ctx
 = let  p       = contextGenPos ctx
        pos     = Pos p
   in   ( ctx   { contextGenPos = p + 1
                , contextElems  = ElemPos pos : contextElems ctx }
        , pos )


-- | Pop elements from a context to get back to the given position.
popToPos :: Pos -> Context n -> Context n
popToPos pos ctx
 = ctx { contextElems = go $ contextElems ctx }
 where
        go []                  = []

        go (ElemPos pos' : ls)
         | pos' == pos          = ls
         | otherwise            = go ls

        go (_ : ls)             = go ls


-- Lookup ---------------------------------------------------------------------
-- | Lookup the type of some variable from the context.
lookupType :: Eq n => Bound n -> Context n -> Maybe (Type n)
lookupType u ctx
 = case u of
        UPrim{}         -> Nothing
        UName n         -> goName n    (contextElems ctx)
        UIx   ix        -> goIx   ix 0 (contextElems ctx)
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


-- | Lookup the kind and role of some type variable from the context.
lookupKind :: Eq n => Bound n -> Context n -> Maybe (Kind n, Role)
lookupKind u ctx
 = case u of
        UPrim{}         -> Nothing
        UName n         -> goName n    (contextElems ctx)
        UIx   ix        -> goIx   ix 0 (contextElems ctx)
 where
        goName _n []    = Nothing
        goName n  (ElemKind (BName n' t) role : ls)
         | n == n'      = Just (t, role)
         | otherwise    = goName n ls
        goName  n (_ : ls)
         = goName n ls


        goIx _ix _d []  = Nothing
        goIx ix d  (ElemKind (BAnon t) role : ls)
         | ix == d      = Just (t, role)
         | otherwise    = goIx   ix (d + 1) ls
        goIx ix d  (_ : ls)
         = goIx ix d ls


-- | Lookup the type bound to an existential, if any.
lookupExistsEq :: Exists -> Context n -> Maybe (Type n)
lookupExistsEq i ctx
 = go (contextElems ctx)
 where  go []                           = Nothing
        go (ElemExistsEq i' t : _)
         | i == i'                      = Just t
        go (_ : ls)                     = go ls


-- Member ---------------------------------------------------------------------
-- | See if this type variable is in the context.
memberType :: Eq n => Bound n -> Context n -> Bool
memberType u ctx = isJust $ lookupType u ctx


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


-- Existentials----------------------------------------------------------------

-- | Get the numeric location of an existential in the context stack,
--   or Nothing if it's not there. Returned value is relative to the TOP
--   of the stack, so the top element has location 0.
locationOfExists 
        :: Exists
        -> Context n
        -> Maybe Int

locationOfExists x ctx
 = go 0 (contextElems ctx)
 where  go !_ix []      = Nothing
        
        go !ix (ElemExistsDecl x'   : moar)
         | x == x'      = Just ix
         | otherwise    = go (ix + 1) moar

        go !ix (ElemExistsEq   x' _ : moar)
         | x == x'      = Just ix
         | otherwise    = go (ix + 1) moar

        go !ix  (_ : moar)
         = go (ix + 1) moar


-- | Update (solve) an existential in the context stack.
updateExists 
        :: [Exists]     -- ^ Other existential declarations to  add before the
                        --   updated one.
        -> Exists       -- ^ Existential to update.
        -> Type n       -- ^ New monotype.
        -> Context n 
        -> Context n

updateExists isMore iEx tEx ctx
 = ctx { contextElems = go $ contextElems ctx }
 where
        go ls
         = case ls of
                []                      -> []
                l@ElemPos{}     : ls'   -> l : go ls'
                l@ElemKind{}    : ls'   -> l : go ls'
                l@ElemType{}    : ls'   -> l : go ls'

                l@(ElemExistsDecl i) : ls'
                 | i == iEx             
                 -> (ElemExistsEq i tEx : [ElemExistsDecl n' | n' <- isMore])
                        ++ ls'

                 | otherwise
                 -> l : go ls'

                l@ElemExistsEq{} : ls'
                 -> l : go ls'


-- Lifting --------------------------------------------------------------------
-- | Lift free debruijn indices in types by the given number of levels.
liftTypes :: Ord n => Int -> Context n -> Context n
liftTypes n ctx
 = ctx { contextElems = go $ contextElems ctx }
 where
        go []                   = []
        go (ElemType b : ls)    = ElemType (liftT n b) : go ls
        go (l:ls)               = l : go ls


-- Lowering --------------------------------------------------------------------
-- | Lower free debruijn indices in types by the given number of levels.
lowerTypes :: Ord n => Int -> Context n -> Context n
lowerTypes n ctx
 = ctx { contextElems = go $ contextElems ctx }
 where
        go []                   = []
        go (ElemType b : ls)    = ElemType (lowerT n b) : go ls
        go (l:ls)               = l : go ls


-- Apply ----------------------------------------------------------------------
-- | Apply a context to a type, updating any existentials in it.
applyContext :: Ord n => Context n -> Type n -> Type n
applyContext ctx tt
 = case tt of
        TVar{}          -> tt

        TCon (TyConExists i _)  
         |  Just t      <- lookupExistsEq (Exists i) ctx
         -> applyContext ctx t

        TCon{}          -> tt

        TForall b t
         -> TForall b (applyContext ctx t)

        TApp t1 t2
         -> TApp (applyContext ctx t1) (applyContext ctx t2)

        TSum ts
         -> TSum $ Sum.fromList (Sum.kindOfSum ts) 
         $  map (applyContext ctx)
         $  Sum.toList ts


-- Support --------------------------------------------------------------------
-- | Check whether this effect is supported by the given context.
--   This is used when effects are treated as capabilities.
--
--   The overall function can be passed a compound effect, 
--    it returns `Nothing` if the effect is supported, 
--    or `Just e`, where `e` is some unsuported atomic effect.
--
effectSupported 
        :: Ord n 
        => Effect n 
        -> Context n 
        -> Maybe (Effect n)

effectSupported eff ctx
        -- Check that all the components of a sum are supported.
        | TSum ts       <- eff
        = listToMaybe $ concat [ maybeToList $ effectSupported e ctx 
                               | e <- Sum.toList ts ]

        -- For an effect on an abstract region, we allow any capability.
        --  We'll find out if it really has this capability when we try
        --  to run the computation.
        | TApp (TCon (TyConSpec tc)) (TVar u) <- eff
        , elem tc [TcConRead, TcConWrite, TcConAlloc]
        , Just (_, RoleAbstract) <- lookupKind u ctx
        = Nothing

        -- Abstract effects are fine.
        --  We'll find out if it is really supported once it's instantiated.
        | TVar {} <- eff
        = Nothing

        -- For an effect on a concrete region,
        --   the capability needs to be in the lexical environment.
        | TApp (TCon (TyConSpec tc)) _t2       <- eff
        , elem tc [TcConRead, TcConWrite, TcConAlloc]
        , elem (ElemType (BNone eff)) (contextElems ctx)
        = Nothing

        | otherwise
        = Just eff
