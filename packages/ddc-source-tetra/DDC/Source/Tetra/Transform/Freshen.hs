{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | Freshen shadowed names in the source file,
--   and rewrite anonymous binders to their named forms.
module DDC.Source.Tetra.Transform.Freshen
        ( type S, evalState, newName
        , freshenModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import Data.Monoid
import Data.Set                         (Set)
import Data.Map.Strict                  (Map)
import Data.Text                        (Text)
import qualified Control.Monad.State    as S
import qualified Data.Text              as Text
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map
import qualified Data.List              as List


-------------------------------------------------------------------------------
-- | Freshen the given module.
freshenModule :: Module Source -> S (Module Source)
freshenModule mm
 = do   tops'   <- freshenTops $ moduleTops mm
        return  $  mm { moduleTops = tops' }


-------------------------------------------------------------------------------
-- | Freshen a top level thing.
freshenTops :: [Top Source] -> S [Top Source]
freshenTops tops
 = do
        let (topCls, topRest)   = List.partition isTopClause tops
        let (sps,    cls)       = unzip $ [(sp, cl) | TopClause sp cl <- topCls]
        cls'    <- freshenClGroup cls
        let topCls'             = zipWith TopClause sps cls'
        return  $ topRest ++ topCls'


-- | Check if this top level thing is a clause.
isTopClause :: Top Source -> Bool
isTopClause tt
 = case tt of
        TopClause{}     -> True
        _               -> False


-------------------------------------------------------------------------------
-- | Freshen a clause group.
freshenClGroup :: [Clause] -> S [Clause]
freshenClGroup cls
 = do   -- TODO: rename shadowed binders

        cls'    <- mapM freshenCl cls
        return  cls'


-- | Freshen a clause.
freshenCl :: Clause -> S Clause
freshenCl cl
 = case cl of
        SSig{}
         -> return cl

        -- TODO: bind params
        SLet a b ps gxs
         -> SLet a b ps <$> mapM freshenGX gxs


-------------------------------------------------------------------------------
-- | Freshen a guarded expression.
freshenGX :: GuardedExp -> S GuardedExp 
freshenGX gx
 = case gx of
        GGuard g gx'
         -> do  -- TODO: decend into g
                gx''    <- freshenGX gx'
                return  $  GGuard g gx''

        GExp x
         -> do  x'      <- freshenX x
                return  $  GExp x'


-------------------------------------------------------------------------------
freshenX :: Exp -> S Exp
freshenX xx
 = case xx of
        XAnnot a x      -> XAnnot a <$> freshenX x
        XVar u          -> XVar     <$> boundUX u
        XPrim{}         -> return xx
        XCon{}          -> return xx

        XLAM (XBindVarMT b mt) x
         -> bindBT b $ \b'
         -> XLAM <$> (XBindVarMT b' <$> freshenMT mt) <*> freshenX x

        XLam (XBindVarMT b mt) x
         -> bindBX b $ \b'
         -> XLam <$> (XBindVarMT b' <$> freshenMT mt) <*> freshenX x

        XApp x1 x2      -> XApp  <$> freshenX x1 <*> freshenX x2

        XLet lts x      
         -> freshBindLts lts $ \lts'
         -> XLet lts' <$> freshenX x

        XCase x alts    -> XCase    <$> freshenX x <*> mapM freshenAC alts
        XCast c x       -> XCast    <$> freshenC c <*> freshenX x
        XType t         -> XType    <$> freshenT t
        XWitness w      -> XWitness <$> freshenW w
        XDefix a xs     -> XDefix a <$> mapM freshenX xs
        XInfixOp{}      -> return xx
        XInfixVar{}     -> return xx
        XMatch a as x   -> XMatch a <$> mapM freshenAM as <*> freshenX x
        XWhere a x cl   -> XWhere a <$> freshenX x <*> freshenClGroup cl

        XLamPat a p mt x
         -> bindP p $ \p' 
         -> XLamPat a p' <$> freshenMT mt <*> freshenX x

        XLamCase a as   
         -> XLamCase a   <$> mapM freshenAC as


-------------------------------------------------------------------------------
-- | Freshen and bind let expressions.
freshBindLts :: Lets -> (Lets -> S a) -> S a
freshBindLts lts cont
 = cont lts
--  TODO: finish this


-------------------------------------------------------------------------------
-- | Freshen a case alternative.
freshenAC :: AltCase -> S AltCase
freshenAC (AAltCase p gxs)
 =  bindP p $ \p'
 -> AAltCase p' <$> mapM freshenGX gxs


-- | Freshen a match alternative
freshenAM :: AltMatch -> S AltMatch
freshenAM (AAltMatch gx)
 = AAltMatch <$> freshenGX gx


-------------------------------------------------------------------------------
-- | Freshen a cast.
freshenC :: Cast -> S Cast
freshenC cc
 = case cc of
        CastWeakenEffect t -> CastWeakenEffect <$> freshenT t
        CastPurify w       -> CastPurify       <$> freshenW w
        CastBox            -> return CastBox
        CastRun            -> return CastRun


-------------------------------------------------------------------------------
-- | Freshen a witness.
freshenW :: Witness -> S Witness
freshenW ww
 = case ww of
        WAnnot a w      -> WAnnot a <$> freshenW w
        WVar  u         -> WVar  <$> boundUX u
        WCon {}         -> return ww
        WApp  w1 w2     -> WApp  <$> freshenW w1 <*> freshenW w2
        WType t         -> WType <$> freshenT t


-------------------------------------------------------------------------------
-- | Freshen a type.
freshenT :: Type -> S Type
freshenT tt
 = case tt of
        TAnnot a t      -> TAnnot a <$> freshenT t

        TCon{}          -> return tt

        TVar u          -> TVar <$> boundUT u

        -- Remember that t1 is outside the scope of 'b'.
        TAbs b t1 t2
         -> do  t1'     <- freshenT t1
                bindBT b $ \b'
                 -> TAbs b' t1' <$> freshenT t2

        TApp t1 t2      -> TApp <$> freshenT t1 <*> freshenT t2


-- | Freshen a Maybe type.
freshenMT :: Maybe Type -> S (Maybe Type)
freshenMT mt
 = case mt of
        Nothing -> return Nothing
        Just t  -> Just <$> freshenT t


-------------------------------------------------------------------------------
-- | Bind a pattern.
bindP  :: Pat -> (Pat -> S a) -> S a
bindP pp cont
 = case pp of
        PDefault
         -> cont pp

        PAt  b p
         -> bindBX b $ \b'
         -> bindP  p $ \p'
         -> cont (PAt b' p')

        PVar b
         -> bindBX b  $ \b'  -> cont (PVar b')

        PData dc ps     
         -> bindPs ps $ \ps' -> cont (PData dc ps')


-- | Bind several patterns in sequence.
bindPs :: [Pat] -> ([Pat] -> S a) -> S a
bindPs ps0 cont
 = go [] ps0
 where
        go psAcc []
         = cont (reverse psAcc)

        go psAcc (p : ps)
         = bindP p $ \p' -> go (p' : psAcc) ps


-------------------------------------------------------------------------------
-- | Bind a new type variable.
bindBT :: Bind -> (Bind -> S a) -> S a
bindBT b@BNone cont
 = cont b

bindBT BAnon cont
 = do   -- Create a new name for anonymous binders.
        name    <- newName "t"

        withModifiedEnvT
         (\envT -> envT { envStack    = name : envStack envT
                        , envStackLen = 1 + envStackLen envT })
         $ cont (BName name)

bindBT (BName n) cont
 =  S.get >>= \state0 
 -> case Set.member n (envNames $ stateEnvT state0) of
        -- If the binder does not shadow an existing one
        -- then don't bother rewriting it.
        False 
         ->     withModifiedEnvT
                 (\envT -> envT { envNames = Set.insert n (envNames envT) })
                 $ cont (BName n)

        -- The binder shadows an existing one, so rewrite it.
        True 
         -> do  name    <- newName "t"

                -- Run the continuation in the environment extended with the new name.
                withModifiedEnvT
                 (\envT -> envT { envNames  = Set.insert   name (envNames  envT) 
                                , envRename = Map.insert n name (envRename envT) })
                 $ cont (BName name)


-------------------------------------------------------------------------------
-- | Bind a new term variable.
bindBX :: Bind -> (Bind -> S a) -> S a
bindBX b@BNone cont
 = cont b

bindBX BAnon cont
 = do   -- Create a new name for anonymous binders.
        name    <- newName "x"

        withModifiedEnvX 
         (\envX -> envX { envStack    = name : envStack envX
                        , envStackLen = 1 + envStackLen envX })
         $ cont (BName name)

bindBX (BName name) cont
 =  S.get >>= \state0 
 -> case Set.member name (envNames $ stateEnvX state0) of
        -- If the binder does not shadow an existing one
        -- then don't bother rewriting it.
        False -> do
                withModifiedEnvX
                 (\envX -> envX { envNames = Set.insert name (envNames envX) })
                 $ cont (BName name)

        -- The binder shadows an existing one, so rewrite it.
        True 
         -> do  name'   <- newName name

                -- Run the continuation in the environment extended with the
                -- new name.
                withModifiedEnvX
                 (\envX -> envX 
                        { envNames  = Set.insert      name' (envNames envX) 
                        , envRename = Map.insert name name' (envRename envX) })
                 $ cont (BName name')


-------------------------------------------------------------------------------
-- | Rewrite bound type variable if needed.
boundUT :: Bound -> S Bound
boundUT uu
 = case uu of
        UName n
         -> do  envT    <- S.gets (envRename   . stateEnvT)
                case Map.lookup n envT of
                 Just n'  -> return $ UName n'
                 _        -> return uu

        UIx i
         -> do  stack   <- S.gets (envStack    . stateEnvT)
                len     <- S.gets (envStackLen . stateEnvT)
                if (i < len)
                 then   return $ UName (stack !! i)
                 else   return uu

        UHole{}
         -> return uu


-------------------------------------------------------------------------------
-- | Rewrite bound term variable if needed.
boundUX :: Bound -> S Bound
boundUX uu
 = case uu of
        UName n
         -> do  envX    <- S.gets (envRename   . stateEnvX)
                case Map.lookup n envX of
                 Just n'  -> return $ UName n'
                 _        -> return uu

        UIx i
         -> do  stack   <- S.gets (envStack    . stateEnvX)
                len     <- S.gets (envStackLen . stateEnvX)
                if (i < len)
                 then   return $ UName (stack !! i)
                 else   return uu

        UHole{}
         -> return uu


-------------------------------------------------------------------------------
-- | State holding a variable name prefix and counter to 
--   create fresh variable names.
type S  = S.State State

data State
        = State
        { -- | Prefix for creating fresh variables.
          stateVarPrefix        :: Text

          -- | Current counter for creating fresh variables.
        , stateVarCount         :: Int

          -- | Environment for type level names.
        , stateEnvT             :: Env 

          -- | Environment for value level names.
        , stateEnvX             :: Env 
        }


-- | Information about a current environemnt.
data Env
        = Env
        { -- | Stack of names of anonymous binders.
          envStack              :: [Name]

          -- | Length of the above sack.
        , envStackLen           :: Int

          -- | Names currently in scope.
        , envNames              :: Set Name

          -- | Names currently being rewritten.
        , envRename             :: Map Name Name }


-- | The empty environmenet.
envZero :: Env
envZero
        = Env
        { envStack              = []
        , envStackLen           = 0
        , envNames              = Set.empty
        , envRename             = Map.empty }


-- | The starting state.
stateZero :: Text -> State
stateZero prefix
        = State
        { stateVarPrefix        = prefix
        , stateVarCount         = 0
        , stateEnvT             = envZero
        , stateEnvX             = envZero }


-- | Evaluate a desguaring computation,
--   using the given prefix for freshly introduced variables.
evalState :: Text -> S a -> a
evalState prefix c
 = S.evalState c (stateZero prefix)


-- | Allocate a new name.
newName :: Text -> S Name
newName pre
 = do   prefix  <- S.gets stateVarPrefix
        count   <- S.gets stateVarCount
        let name = pre <> "$" <> prefix <> Text.pack (show count)
        S.modify $ \s -> s { stateVarCount = count + 1 }
        return  name


-- | Run a computation in a modified EnvT, 
--   restoring the original environment after it's done.
withModifiedEnvT :: (Env -> Env) -> S a -> S a
withModifiedEnvT modEnvT cont
 = do
        state     <- S.get
        let envT  =  stateEnvT state
        let envT' =  modEnvT envT
        S.put state { stateEnvT = envT' }

        result  <- cont

        state'  <- S.get
        S.put state' { stateEnvT = envT }
        return result


-- | Run a computation in a modified EnvX, 
--   restoring the original environment after it's done.
withModifiedEnvX :: (Env -> Env) -> S a -> S a
withModifiedEnvX modEnvX cont
 = do
        state     <- S.get
        let envX  =  stateEnvX state
        let envX' =  modEnvX envX
        S.put state { stateEnvX = envX' }

        result  <- cont

        state'  <- S.get
        S.put state' { stateEnvX = envX }
        return result

