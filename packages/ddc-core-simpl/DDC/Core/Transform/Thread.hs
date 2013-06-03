
-- | Thread a state token through calls to given functions.
module DDC.Core.Transform.Thread
        ( Thread (..)
        , Config (..))
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Transform.Reannotate
import DDC.Core.Check           (AnTEC (..))
import DDC.Type.Env             (KindEnv, TypeEnv)
import qualified DDC.Type.Env   as Env
import qualified DDC.Core.Check as Check


-------------------------------------------------------------------------------
-- | Configuration for the Thread transform.
data Config a n
        = Config
        { -- | Config for the type checker.
          --   We need to reconstruct the type of the result of stateful
          --   functions when bundling them into the tuple that holds the 
          --   state token.
          configCheckConfig     :: Check.Config n

          -- | Function to decide which top-level bindings are stateful and
          --   need the state token threaded through them. If the binding with
          --   the given name is stateful then the function should return the
          --   new type for the binding that accepts and returns the state token.
        , configThreadMe        :: n -> Maybe (Type n) 

          -- | Type of the state token to use.
        , configTokenType       :: Type n

          -- | Type that represents a missing value.
          --   If a stateful function returns a void then our thread transform
          --   rewrites it to return the state token, instead of a tuple
          --   that contains the token as well as a void value.
        , configVoidType        :: Type n

          -- | Wrap a type with the world token.
          --   eg change Int to (World#, Int)
        , configWrapResultType  :: Type n -> Type n

          -- | Wrap a result expression with the state token.
          --   The function is given the types of the world token and result,
          --   then the expressions for the same.
        , configWrapResultExp   :: Exp (AnTEC a n) n  -> Exp (AnTEC a n) n 
                                -> Exp a n

          -- | Make a pattern which binds the world argument
          --   from a threaded primop.
        , configThreadPat       :: n -> Maybe (Bind n -> Bind n -> Pat n)
        }


-- | Class of things that can have a state token threaded through them.
class Thread (c :: * -> * -> *) where
 thread :: (Ord n, Show n, Pretty n)
        => Config a n 
        -> KindEnv n -> TypeEnv n 
        -> c (AnTEC a n) n     
        -> c a n


instance Thread Module where
 thread config kenv tenv mm
  = let body'   = threadModuleBody config kenv tenv (moduleBody mm) 
    in  mm { moduleBody = body' }


-- Module ---------------------------------------------------------------------
-- | Thread state token though a module body.
--   We assume every top-level binding is a stateful function
--   that needs to accept and return the state token.
threadModuleBody 
        :: (Ord n, Show n, Pretty n)
        => Config a n 
        -> KindEnv n -> TypeEnv n
        -> Exp (AnTEC a n) n   
        -> Exp a n

threadModuleBody config kenv tenv xx
 = case xx of
        XLet a lts x
         -> let lts'       = threadTopLets    config kenv tenv lts
                (bks, bts) = bindsOfLets lts
                kenv'      = Env.extends bks kenv
                tenv'      = Env.extends bts tenv
                x'         = threadModuleBody config kenv' tenv' x
            in  XLet (annotTail a) lts' x'

        _ -> reannotate annotTail xx


-- | Thread state token through some top-level bindings in a module.
threadTopLets    
        :: (Ord n, Show n, Pretty n)
        => Config a n 
        -> KindEnv n -> TypeEnv n
        -> Lets (AnTEC a n) n  
        -> Lets a n

threadTopLets config kenv tenv lts
 = case lts of
        LLet m b x
         -> let (b', x')  = threadTopBind  config kenv tenv b x
            in  LLet m b' x'

        LRec bxs
         -> let tenv'     =   Env.extends (map fst bxs) tenv
                bxs'      = [ threadTopBind config kenv tenv' b x 
                                | (b, x) <- bxs]
            in  LRec bxs'

        _ -> reannotate annotTail lts


-- TopBind ------------------------------------------------------------------
-- | Thread state token into a top-level binding.
--   We assume every top-level binding is stateful function that needs to
--   accept and return the state token.
--
--   We inject the world type into the type of the function and then call
--   threadBind which will add the actual lambda for the new argument.
--
threadTopBind
        :: (Ord n, Show n, Pretty n)
        => Config a n
        -> KindEnv n -> TypeEnv n
        ->  Bind n   -> Exp (AnTEC a n) n
        -> (Bind n,     Exp a n)

threadTopBind config kenv tenv b xBody
 = let  tBind   = typeOfBind b
        tBind'  = injectStateType config tBind
        b'      = replaceTypeOfBind tBind' b
        tenv'   = Env.extend b' tenv
        tsArgs  = fst $ takeTFunAllArgResult tBind'
   in   ( b'
        , threadProc config kenv tenv' xBody tsArgs)


-- Arg ------------------------------------------------------------------------
-- | Thread state token into an argument expression.
--   If it is a syntactic function then we assume the function is stateful
--   and needs the state token added, otherwise return it unharmed.
threadArg 
        :: (Ord n, Show n, Pretty n)
        => Config a n
        -> KindEnv n -> TypeEnv n
        -> Type n    -> Exp (AnTEC a n) n
        -> Exp a n

threadArg config kenv tenv t xx
 = case xx of
        XLam{}  -> threadProcArg config kenv tenv t xx
        XLAM{}  -> threadProcArg config kenv tenv t xx
        _       -> reannotate annotTail xx

threadProcArg config kenv tenv t xx
 = let  tsArgs  = fst $ takeTFunAllArgResult t
   in   threadProc config kenv tenv xx tsArgs


-- Proc -----------------------------------------------------------------------
-- | Thread world token into the body of a stateful function (procedure).
threadProc
        :: (Ord n, Show n, Pretty n)
        => Config a n
        -> KindEnv n -> TypeEnv n
        -> Exp (AnTEC a n) n    -- Whole expression, including lambdas.
        -> [Type n]             -- Types of function parameters.
        -> Exp a n

-- We're out of parameters. 
--  Now thread into the statements in the function body.
threadProc config kenv tenv xx []
 = threadProcBody config kenv tenv xx

-- We're still decending past all the lambdas.
--  When we get to the inner-most one then add the state parameter.
threadProc config kenv tenv xx (t : tsArgs)
 = case xx of
        -- TODO: check arg type matches
        XLAM a b x
          -> let kenv'  = Env.extend b kenv
                 x'     = threadProc config kenv' tenv x tsArgs
             in  XLAM (annotTail a) b x'

        -- TODO: check arg type matches.
        XLam a b x      
          -> let tenv'  = Env.extend b tenv
                 x'     = threadProc config kenv tenv' x tsArgs
             in  XLam (annotTail a) b x'

        -- Inject a new lambda to bind the state parameter.
        _ |  Just a     <- takeAnnotOfExp xx
          ,  t == configTokenType config 
          -> let b'     = BAnon (configTokenType config)
                 tenv'  = Env.extend b' tenv
                 x'     = threadProc config kenv tenv' xx tsArgs
             in  XLam (annotTail a) b' x'

        -- We've decended past all the lambdas,
        -- so now thread into the procedure body.
        _ -> threadProcBody config kenv tenv xx


-- | Thread world token into a binding that isn't a function.
threadProcBody 
        :: (Ord n, Show n, Pretty n)
        => Config a n 
        -> KindEnv n -> TypeEnv n
        -> Exp (AnTEC a n) n   
        -> Exp a n

threadProcBody config kenv tenv xx
 = case xx of
 
        -- A statement in the procedure body.
        XLet _ (LLet _ b x) x2
         |  Just (XVar a (UPrim nPrim _), xsArgs) 
                         <- takeXApps x
         ,  Just tNew    <- configThreadMe  config nPrim
         ,  Just mkPat   <- configThreadPat config nPrim
         -> let 
                tWorld  = configTokenType config

                -- Add world token as final argument 
                xsArgs' = xsArgs ++ [XVar a (UIx 0)]

                -- Thread into possibly higher order arguments.
                tsArgs   = fst $ takeTFunAllArgResult tNew
                xsArgs'' = zipWith (threadArg config kenv tenv) tsArgs xsArgs'

                -- Build the final expression.
                x'      = xApps (annotTail a) (XVar (annotTail a) (UPrim nPrim tNew)) xsArgs''

                -- Thread into let-expression body.
                tenv'   = Env.extend b tenv
                x2'     = threadProcBody config kenv tenv' x2
                pat'    = mkPat (BAnon tWorld) b
            in  XCase (annotTail a) x' [AAlt pat' x2']

        -- A pure binding that doesn't need the token.
        XLet a lts x
         -> let (bks, bts) = bindsOfLets lts
                kenv'   = Env.extends bks kenv
                tenv'   = Env.extends bts tenv
                lts'    = reannotate annotTail lts
                x'      = threadProcBody config kenv' tenv' x
            in  XLet (annotTail a) lts' x'

        -- Assume it's a pure case for now, and that it's been ANF'd.
        XCase a x alts
         -> let alts' = map (threadAlt config kenv tenv) alts
                x'    = threadProcBody config kenv tenv x
            in  XCase (annotTail a) x' alts'

        -- TODO: convert this to Nothing, proper exception.
        XLAM{}          -> error "ddc-core-simpl: threadProcBody unexpected XLAM"
        XLam{}          -> error "ddc-core-simpl: threadProcBody unexpected XLam"
        XCast{}         -> error "ddc-core-simpl: threadProcBody unexpected XCast"
        XType t         -> XType t
        XWitness w      -> XWitness w

        -- For XVar, XCon, XApp as result value of function.
        _
         -> let Just a  = takeAnnotOfExp xx
                a'      = AnTEC (configTokenType config) 
                                (tBot kEffect) 
                                (tBot kClosure)
                                (annotTail a)

                xWorld  = XVar a' (UIx 0)
                wrap    = configWrapResultExp config
                
            in  wrap xWorld xx

-- | Thread world token into a case alternative
threadAlt 
        :: (Ord n, Show n, Pretty n)
        => Config a n 
        -> KindEnv n -> TypeEnv n
        -> Alt (AnTEC a n) n   
        -> Alt a n

threadAlt config kenv tenv (AAlt pat xx)
 = case pat of
        PDefault
         -> AAlt pat (threadProcBody config kenv tenv xx)
        PData _ bs
         -> let tenv' = Env.extends bs tenv
            in  AAlt pat (threadProcBody config kenv tenv' xx)
 

-------------------------------------------------------------------------------
-- | Inject the state token into the type of an effectful function.
--   Eg, change  ([a b : Data]. a -> b -> Int) 
--          to   ([a b : Data]. a -> b -> World -> (World, Int)
injectStateType :: Eq n => Config a n -> Type n -> Type n
injectStateType config tt
 = let down = injectStateType config
   in case tt of
        TForall b x     
         -> TForall b (down x)

        TApp{}
         | (tsArg@(_ : _), tResult)     <- takeTFunArgResult tt
         -> let  tsArg'   = tsArg ++ [configTokenType config]
                 tResult' = injectStateType config tResult
            in   foldr tFunPE tResult' tsArg'

        _ | tt == configTokenType config -> tt
          | tt == configVoidType  config -> configTokenType config
          | otherwise                    -> configWrapResultType config tt

