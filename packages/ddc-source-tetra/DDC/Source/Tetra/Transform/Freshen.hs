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
        let (topCls, topRest) = List.partition isTopClause tops
        let (sps,    cls)     = unzip $ [(sp, cl) | TopClause sp cl <- topCls]
        cls'        <- freshenClauseGroup cls
        let topCls' =  zipWith TopClause sps cls'
        return  $ topRest ++ topCls'


-- | Check if this top level thing is a clause.
isTopClause :: Top Source -> Bool
isTopClause tt
 = case tt of
        TopClause{}     -> True
        _               -> False


-------------------------------------------------------------------------------
-- | Freshen a clause group.
freshenClauseGroup :: [Clause] -> S [Clause]
freshenClauseGroup cls
 = do   -- TODO: rename shadowed binders
        -- remembering that we could have multiple clauses that use
        -- the same binder.

        cls'    <- mapM freshenClause cls
        return  cls'


-- | Freshen a clause.
freshenClause :: Clause -> S Clause
freshenClause cl
 = case cl of
        SSig{}
         -> return cl

        SLet a b ps gxs
         -> mapFreshBinds bindParam ps $ \ps'
         -> SLet a b ps' <$> mapM freshenGuardedExp gxs


-------------------------------------------------------------------------------
-- | Freshen and bind a function parameter.
bindParam :: Param -> (Param -> S a) -> S a
bindParam pp cont
 = case pp of
        MType b mt
         -> bindBT b $ \b'
         -> do  mt'     <- traverse freshenType mt
                cont $ MType b' mt'

        MWitness b mt
         -> bindBX b $ \b'
         -> do  mt'     <- traverse freshenType mt
                cont $ MWitness b' mt'

        MValue p mt
         -> bindPat p $ \p'
         -> do  mt'     <- traverse freshenType mt
                cont $ MValue p' mt'


-------------------------------------------------------------------------------
-- | Freshen a guarded expression.
freshenGuardedExp :: GuardedExp -> S GuardedExp 
freshenGuardedExp gx
 = case gx of
        GGuard g gx'
         -> bindGuard g $ \g'
         -> GGuard g' <$> freshenGuardedExp gx'

        GExp x
         -> do  x'      <- freshenExp x
                return  $  GExp x'


-------------------------------------------------------------------------------
-- | Freshen an expression.
freshenExp :: Exp -> S Exp
freshenExp xx
 = case xx of
        XAnnot a x      -> XAnnot a <$> freshenExp x
        XVar u          -> XVar     <$> boundUX u
        XPrim{}         -> return xx
        XCon{}          -> return xx

        XLAM (XBindVarMT b mt) x
         -> do  mt'     <- traverse freshenType mt
                bindBT b $ \b'
                 -> XLAM (XBindVarMT b' mt') <$> freshenExp x

        XLam (XBindVarMT b mt) x
         -> do  mt'     <- traverse freshenType mt
                bindBX b $ \b'
                 -> XLam (XBindVarMT b' mt') <$> freshenExp x

        XApp x1 x2      -> XApp  <$> freshenExp x1 <*> freshenExp x2

        XLet lts x      
         -> bindLets lts $ \lts'
         -> XLet lts' <$> freshenExp x

        XCase x alts    -> XCase    <$> freshenExp x 
                                    <*> mapM freshenAltCase alts

        XCast c x       -> XCast    <$> freshenCast c <*> freshenExp x

        XType t         -> XType    <$> freshenType t

        XWitness w      -> XWitness <$> freshenWitness w

        XDefix a xs     -> XDefix a <$> mapM freshenExp xs

        XInfixOp{}      -> return xx

        XInfixVar{}     -> return xx

        XMatch a as x   -> XMatch a <$> mapM freshenAltMatch as 
                                    <*> freshenExp x

        XWhere a x cl   -> XWhere a <$> freshenExp x <*> freshenClauseGroup cl

        XLamPat a p mt x
         -> do  mt'     <- traverse freshenType mt
                bindPat p $ \p' 
                 -> XLamPat a p' mt' <$> freshenExp x

        XLamCase a as   
         -> XLamCase a   <$> mapM freshenAltCase as


-------------------------------------------------------------------------------
-- | Freshen and bind guards.
bindGuard   :: Guard -> (Guard -> S a) -> S a
bindGuard gg cont
 = case gg of
        GPat p x       
         -> bindPat p $ \p'
         -> cont =<< (GPat p' <$> freshenExp x)

        GPred x  
         -> cont =<< (GPred   <$> freshenExp x)

        GDefault 
         -> cont GDefault


-------------------------------------------------------------------------------
-- | Freshen and bind let expressions.
bindLets :: Lets -> (Lets -> S a) -> S a
bindLets lts cont
 = case lts of
        LLet (XBindVarMT b mt) x
         -> do  mt'     <- traverse freshenType mt        
                bindBX b $ \b' 
                 -> cont =<< (LLet (XBindVarMT b' mt') <$> freshenExp x)

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                mapFreshBinds bindBVX bs $ \bs'
                 -> do  xs'     <- mapM freshenExp xs
                        cont (LRec $ zip bs' xs')

        LPrivate brs mt bwts
         -> do  mt'     <- traverse freshenType mt
                mapFreshBinds bindBT brs $ \brs'
                 -> do  let (bws, ts)  = unzip bwts
                        ts'     <- mapM freshenType ts
                        mapFreshBinds bindBX bws $ \bws'
                         -> cont (LPrivate brs' mt' $ zip bws' ts')

        LGroup cls
         -> cont =<< (LGroup <$> freshenClauseGroup cls)


-------------------------------------------------------------------------------
-- | Freshen a case alternative.
freshenAltCase :: AltCase -> S AltCase
freshenAltCase (AAltCase p gxs)
 =  bindPat p $ \p'
 -> AAltCase p' <$> mapM freshenGuardedExp gxs


-- | Freshen a match alternative
freshenAltMatch :: AltMatch -> S AltMatch
freshenAltMatch (AAltMatch gx)
 = AAltMatch <$> freshenGuardedExp gx


-------------------------------------------------------------------------------
-- | Freshen a cast.
freshenCast :: Cast -> S Cast
freshenCast cc
 = case cc of
        CastWeakenEffect t -> CastWeakenEffect <$> freshenType t
        CastPurify w       -> CastPurify       <$> freshenWitness w
        CastBox            -> return CastBox
        CastRun            -> return CastRun


-------------------------------------------------------------------------------
-- | Freshen a witness.
freshenWitness :: Witness -> S Witness
freshenWitness ww
 = case ww of
        WAnnot a w      -> WAnnot a <$> freshenWitness w
        WVar  u         -> WVar  <$> boundUX u
        WCon {}         -> return ww
        WApp  w1 w2     -> WApp  <$> freshenWitness w1 <*> freshenWitness w2
        WType t         -> WType <$> freshenType t


-------------------------------------------------------------------------------
-- | Freshen a type.
freshenType :: Type -> S Type
freshenType tt
 = case tt of
        TAnnot a t      -> TAnnot a <$> freshenType t

        TCon{}          -> return tt

        TVar u          -> TVar <$> boundUT u

        TAbs b t1 t2
         -> do  t1'     <- freshenType t1
                bindBT b $ \b'
                 -> TAbs b' t1' <$> freshenType t2

        TApp t1 t2      -> TApp <$> freshenType t1 <*> freshenType t2


-------------------------------------------------------------------------------
-- | Bind a pattern.
bindPat  :: Pat -> (Pat -> S a) -> S a
bindPat pp cont
 = case pp of
        PDefault
         -> cont pp

        PAt  b p
         -> bindBX  b $ \b'
         -> bindPat p $ \p'
         -> cont (PAt b' p')

        PVar b
         -> bindBX b  $ \b'
         -> cont (PVar b')

        PData dc ps     
         -> mapFreshBinds bindPat ps $ \ps' 
         -> cont (PData dc ps')


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
      -> do name    <- newName "t"

            -- Run the continuation in the extended environment.
            withModifiedEnvT
             (\envT -> envT { envNames  = Set.insert   name (envNames  envT) 
                            , envRename = Map.insert n name (envRename envT)})
             $ cont (BName name)


-------------------------------------------------------------------------------
-- | Bind a term variable with its attached type.
bindBVX :: BindVarMT -> (BindVarMT -> S a) -> S a
bindBVX (XBindVarMT b mt) cont
 = do   mt'     <- traverse freshenType mt
        bindBX b $ \b'
         -> cont (XBindVarMT b' mt')


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
                 (\envX -> envX { envNames = Set.insert name (envNames envX)})
                 $ cont (BName name)

        -- The binder shadows an existing one, so rewrite it.
        True 
         -> do  name'   <- newName name

                -- Run the continuation in the environment extended with the
                -- new name.
                withModifiedEnvX
                 (\envX -> envX 
                        { envNames  = Set.insert      name' (envNames  envX) 
                        , envRename = Map.insert name name' (envRename envX)})
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


-- | Given a function that binds and freshens a single thing,
--   binds and freshens a list of things in sequence.
mapFreshBinds 
        :: (a  -> ( a  -> S b) -> S b)
        -> [a] -> ([a] -> S b) -> S b

mapFreshBinds freshBind as0 cont
 = go [] as0
 where
        go asAcc []
         = cont (reverse asAcc)

        go asAcc (a : as)
         = freshBind a $ \a' -> go (a' : asAcc) as

