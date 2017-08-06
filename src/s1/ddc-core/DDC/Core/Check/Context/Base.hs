{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Context.Base
        ( Context (..)

          -- * Construction
        , emptyContext
        , contextOfEnvT
        , contextOfEnvX
        , contextOfPrimEnvs

          -- * Projection
        , contextEquations
        , contextCapabilities
        , contextDataDefs
        , contextEnvT

          -- * Pushing
        , pushType,   pushTypes
        , pushKind,   pushKinds
        , pushExists
        , pushExistsBefore
        , pushExistsScope

          -- * Marking
        , markContext

          -- * Popping
        , popToPos

          -- * Lookup
        , lookupType
        , lookupKind
        , lookupExistsEq

          -- * Membership
        , memberType
        , memberKind
        , memberKindBind

          -- * Existentials
        , locationOfExists
        , updateExists

          -- * Lifting
        , liftTypes
        , lowerTypes)
where
import DDC.Core.Check.Context.Elem
import DDC.Core.Env.EnvX                (EnvX)
import DDC.Core.Env.EnvX                (EnvT)
import DDC.Type.Transform.BoundT
import DDC.Type.Exp.Simple
import DDC.Type.DataDef
import DDC.Data.Pretty
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified DDC.Type.Env           as Env

import Data.Maybe
import Data.IntMap.Strict               (IntMap)
import Data.Map                         (Map)
import qualified Data.IntMap.Strict     as IntMap

import Prelude                          hiding ((<$>))



-- Context --------------------------------------------------------------------
-- | The type checker context.
--
--   Holds a position counter and a stack of elements.
--   The top of the stack is at the front of the list.
--
data Context n
        = Context
        { -- | Top level environment for terms.
          contextEnvX           :: !(EnvX n)

          -- | Fresh name generator for context positions.
        , contextGenPos         :: !Int

          -- | Fresh name generator for existential variables.
        , contextGenExists      :: !Int

          -- | The current context stack.
        , contextElems          :: ![Elem n]

          -- | Types of solved existentials.
          --   When solved constraints are popped from the main context stack
          --   they are added to this map. The map is used to fill in type
          --   annotations after type inference proper. It's not used as part
          --   of the main algorithm.
        , contextSolved         :: IntMap (Type n) }


instance (Pretty n, Eq n) => Pretty (Context n) where
 ppr (Context _ genPos genExists ls _solved)
  =   text "Context "
  <$> text "  genPos    = " <> int genPos
  <$> text "  genExists = " <> int genExists
  <$> indent 2
        (vcat $ [padL 4 (int i)  <+> ppr l
                        | l <- reverse ls
                        | i <- [0..]])



-- Construction ---------------------------------------------------------------
-- | An empty context.
emptyContext :: Context n
emptyContext
        = Context
        { contextEnvX           = EnvX.empty
        , contextGenPos         = 0
        , contextGenExists      = 0
        , contextElems          = []
        , contextSolved         = IntMap.empty }


-- | Wrap an EnvT into a Context.
contextOfEnvT :: EnvT n -> Context n
contextOfEnvT envt
 = let  envx    = EnvX.empty
                { EnvX.envxEnvT = envt }
   in   contextOfEnvX envx


-- | Wrap an `EnvX` into a context.
contextOfEnvX :: EnvX n -> Context n
contextOfEnvX envx
        = emptyContext { contextEnvX = envx }


-- | Build a context from prim environments.
contextOfPrimEnvs
        :: Ord n
        => Env.KindEnv n
        -> Env.TypeEnv n
        -> DataDefs n
        -> Context n

contextOfPrimEnvs kenv tenv defs
        = emptyContext
        { contextEnvX = EnvX.fromPrimEnvs kenv tenv defs }


-- Projection -----------------------------------------------------------------
-- | Take the type equations from a context.
contextEquations :: Context n -> Map n (Type n)
contextEquations ctx
 = EnvT.envtEquations   $ contextEnvT ctx


-- | Take the capabilities from a context.
contextCapabilities :: Context n -> Map n (Type n)
contextCapabilities ctx
 = EnvT.envtCapabilities $ contextEnvT ctx


-- | Take the capabilities from a context.
contextDataDefs :: Context n -> DataDefs n
contextDataDefs ctx
 = EnvX.envxDataDefs $ contextEnvX ctx


-- | Take the top level environment for types from the context.
contextEnvT :: Context n -> EnvT n
contextEnvT ctx
 = EnvX.envxEnvT (contextEnvX ctx)


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
pushExists :: Exists n -> Context n -> Context n
pushExists i ctx
 = ctx { contextElems = ElemExistsDecl i : contextElems ctx }


-- | Push the first existential into the context just before the
--   declaration of the second one.
pushExistsBefore :: Exists n -> Exists n -> Context n -> Context n
pushExistsBefore i (Exists n _) ctx
 = ctx { contextElems = go (contextElems ctx) }
 where
        go (ElemExistsDecl i'@(Exists n' _)   : es)
         | n' == n      = ElemExistsDecl i'   : ElemExistsDecl i : es

        go (ElemExistsEq   i'@(Exists n' _) t : es)
         | n' == n      = ElemExistsEq   i' t : ElemExistsDecl i : es

        go (e : es)     = e : go es
        go []           = []


pushExistsScope :: Exists n -> [Exists n] -> Context n -> Context n
pushExistsScope i scope ctx
 = ctx { contextElems
                = go    (ElemExistsDecl i : contextElems ctx)
                        []
                        (contextElems ctx)
       }
 where
        go cs' acc (e : es)
         | Just i' <- takeExistsOfElem e
         , elem i' scope
         = go (reverse acc ++ (e : ElemExistsDecl i : es)) (e : acc) es

         | otherwise
         = go cs' (e : acc) es

        go cs' _acc []
         = cs'


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
-- | Given a bound level-0 (value) variable, lookup its type (level-1)
--   from the context.
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


-- | Given a bound level-1 (type) variable, lookup its kind (level-2) from
--   the context.
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
lookupExistsEq :: Exists n -> Context n -> Maybe (Type n)
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
        :: Exists n
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
--
--   If the existential is not part of the context then `Nothing`.
updateExists
        :: [Exists n]   -- ^ Other existential declarations to  add before the
                        --   updated one.
        -> Exists n     -- ^ Existential to update.
        -> Type n       -- ^ New monotype.
        -> Context n
        -> Maybe (Context n)

updateExists isMore iEx@(Exists iEx' _) tEx ctx
 = case go $ contextElems ctx of
    Just elems'
     -> Just $ ctx { contextElems  = elems'
                   , contextSolved = IntMap.insert iEx' tEx (contextSolved ctx) }
    Nothing -> Nothing
 where
        go ll
         = case ll of
                l@ElemPos{}     : ls
                 | Just ls'     <- go ls  -> Just (l : ls')

                l@ElemKind{}    : ls
                 | Just ls'     <- go ls  -> Just (l : ls')

                l@ElemType{}    : ls
                 | Just ls'     <- go ls  -> Just (l : ls')

                l@(ElemExistsDecl i) : ls
                 | i == iEx
                 -> let es  =  ElemExistsEq i tEx
                            : [ElemExistsDecl n' | n' <- isMore]
                    in  Just $ es ++ ls

                 | Just ls'     <- go ls  -> Just (l : ls')

                l@ElemExistsEq{} : ls
                 | Just ls'     <- go ls  -> Just (l : ls')

                _ -> Just ll  -- Nothing


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
