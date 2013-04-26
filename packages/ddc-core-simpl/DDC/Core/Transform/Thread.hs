
-- | Thread a state token through calls to given functions.
module DDC.Core.Transform.Thread
        ( Thread (..)
        , Config (..))
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.DataDef

-------------------------------------------------------------------------------
data Config a n
        = Config
        { -- | Data type definitions
          configDataDefs        :: DataDefs n

          -- | Type of the token to use.
        , configTokenType       :: Type n

          -- | Type that represents a missing value.
        , configVoidType        :: Type n

          -- | Wrap a type with the world token
          --   eg change Int to (World#, Int)
        , configWrapResultType  :: Type n -> Type n

          -- | Wrap a result expression with the world token
        , configWrapResultExp   :: Exp a n  -> Exp a n -> Exp a n

          -- | If a name maps to a new type,
          --   then pass the token for all matching arguments.
        , configThreadMe        :: n -> Maybe (Type n) 

          -- | Make a pattern which binds the world argument
          --   from a threaded primop.
        , configThreadPat       :: n -> Maybe (Bind n -> Bind n -> Pat n)
        }


class Thread (c :: * -> * -> *) where
 thread :: Eq n => Config a n -> c a n -> c a n


instance Thread Module where
 thread config mm
  = let body'   = threadModuleBody config (moduleBody mm) 
    in  mm { moduleBody = body' }


-- ModuleBody -----------------------------------------------------------------
-- | Thread state token though a module body.
--   We assume every top-level binding is a stateful function
--   that needs to accept and return the state token.
threadModuleBody :: Eq n => Config a n -> Exp a n -> Exp a n
threadModuleBody config xx
 = case xx of
        XLet a lts x
          -> XLet a (threadTopLets config lts) 
                    (threadModuleBody config x)
        _ -> xx

threadTopLets    :: Eq n => Config a n -> Lets a n -> Lets a n
threadTopLets config lts
 = case lts of
        LLet m b x
         -> let (b', x')  = threadTopBind config b x
            in  LLet m b' x'

        LRec bxs
         -> let bxs'      = [threadTopBind config b x | (b, x) <- bxs]
            in  LRec bxs'

        _ -> lts


-- TopBind ------------------------------------------------------------------
-- | Thread state token into a top-level binding.
--   We assume every top-level binding is stateful function that needs to
--   accept and return the state token.
--
--   We inject the world type into the type of the function and then call
--   threadBind which will add the actual lambda for the new argument.
--
threadTopBind
        :: Eq n
        => Config a n
        ->  Bind n -> Exp a n
        -> (Bind n,   Exp a n)

threadTopBind config b xBody
 = let  tBind   = typeOfBind b
        tBind'  = injectStateType config tBind
        b'      = replaceTypeOfBind tBind' b
        tsArgs  = fst $ takeTFunAllArgResult tBind'
   in   ( b'
        , threadProc config xBody tsArgs)


-- Arg ------------------------------------------------------------------------
-- | Thread state token into an argument expression.
--   If it is a syntactic function then we assume the function is stateful
--   and needs the state token added, otherwise return it unharmed.
threadArg 
        :: Eq n
        => Config a n
        -> Type n -> Exp a n
        -> Exp a n

threadArg config t xx
 = case xx of
        XLam{}  -> threadProcArg config t xx
        XLAM{}  -> threadProcArg config t xx
        _       -> xx

threadProcArg config t xx
 = let  tsArgs  = fst $ takeTFunAllArgResult t
   in   threadProc config xx tsArgs


-- Proc -----------------------------------------------------------------------
-- | Thread world token into the body of a stateful function (procedure).
threadProc
        :: Eq n
        => Config a n
        -> Exp a n      -- Whole expression, including lambdas.
        -> [Type n]     -- Types of function parameters.
        -> Exp a n

-- We're out of parameters. 
--  Now thread into the statements in the function body.
threadProc config xx []
 = threadProcBody config xx

-- We're still decending past all the lambdas.
--  When we get to the inner-most one then add the state parameter.
threadProc config xx (t : tsArgs)
 = case xx of
        -- TODO: check arg type matches
        XLAM a b x
          -> XLAM a b (threadProc config x tsArgs)

        -- TODO: check arg type matches.
        XLam a b x      
          -> XLam a b (threadProc config x tsArgs)

        -- Inject a new lambda to bind the state parameter.
        _ |  Just a     <- takeAnnotOfExp xx
          ,  t == configTokenType config 
          -> XLam a (BAnon (configTokenType config))
                    (threadProc config xx tsArgs)

        -- We've decended past all the lambdas,
        -- so now thread into the procedure body.
        _ -> threadProcBody config xx


-- | Thread world token into a binding that isn't a function.
threadProcBody :: Eq n => Config a n -> Exp a n -> Exp a n
threadProcBody config xx
 = case xx of
 
        -- A statement in the procedure body.
        XLet _ (LLet _ b x) x2
         |  Just (XVar a (UPrim nPrim _), xsArgs) 
                         <- takeXApps x
         ,  Just tNew    <- configThreadMe  config nPrim
         ,  Just mkPat   <- configThreadPat config nPrim
         -> let 
                tWorld  = configTokenType config

                -- Thread into possibly higher order arguments.
                tsArgs  = fst $ takeTFunAllArgResult tNew
                xsArgs' = zipWith (threadArg config) tsArgs xsArgs

                -- Add world token as final argument 
                xsArgs_world = xsArgs' ++ [XVar a (UIx 0)]
                x'      = xApps a (XVar a (UPrim nPrim tNew)) xsArgs_world

                -- Thread into let-expression body.
                x2'     = threadProcBody config x2
                pat'    = mkPat (BAnon tWorld) b
            in  XCase a x' [AAlt pat' x2']

        -- A pure binding that doesn't need the token.
        XLet a lts x
         -> let x'      = threadProcBody config x
            in  XLet a lts x'

        XCase{}         -> error "ddc-core-simpl: thread not finished"

        -- TODO: convert this to Nothing, proper exception.
        XLAM{}          -> error "ddc-core-simpl: death XLAM"
        XLam{}          -> error "ddc-core-simpl: death XLam"
        XCast{}         -> error "ddc-core-simpl: death XCast"
        XType{}         -> xx
        XWitness{}      -> xx

        -- For XVar, XCon, XApp as result value of function.
        _
         -> let Just a  = takeAnnotOfExp xx
                xWorld  = XVar a (UIx 0)
                wrap    = configWrapResultExp config
            in  wrap xWorld xx


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




