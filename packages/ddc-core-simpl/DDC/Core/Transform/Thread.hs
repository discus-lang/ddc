
-- | Thread a state token through calls to given functions.
module DDC.Core.Transform.Thread
        ( Thread (..)
        , Config (..))
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.DataDef

data Config a n
        = Config
        { -- | Data type definitions
          configDataDefs        :: DataDefs n

          -- | Type of the token to use.
        , configTokenType       :: Type n

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
--    We assume every top-level binding is a stateful function
--    that needs to accept and return the state token.
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


-- TopBind --------------------------------------------------------------------
-- | Thread state token into a top-level binding.
--    We're just assuming every top-level binding is stateful function
--    and needs to accept and return the state token.
--
--    We inject the world type into the type of the function and then call
--    threadBind which will add the actual lambda for the new argument.
--
threadTopBind
        :: Eq n
        => Config a n
        -> Bind n
        -> Exp a n
        -> (Bind n, Exp a n)

threadTopBind config b xBody
 = let  tBind   = typeOfBind b
        tBind'  = injectStateType config tBind
        b'      = replaceTypeOfBind tBind' b
   in   ( b'
        , threadBind config b' xBody)


-- | Inject the state token into the type of a top-level function.
--   Eg, change  ([a b : Data]. a -> b -> Int) 
--          to   ([a b : Data]. a -> b -> World -> (World, Int)
injectStateType :: Config a n -> Type n -> Type n
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

        _ -> configWrapResultType config tt


-- Bind ------------------------------------------------------------------------
-- | Thread world tokens into a let binding.
threadBind :: Eq n => Config a n -> Bind n -> Exp a n -> Exp a n
threadBind config bind xBody
 | tBind                        <- typeOfBind bind
 , Just (bsQuant, tBody)        <- takeTForalls tBind
 , (tsArg,   _tsResult)         <- takeTFunArgResult tBody
 = let  tks     =  map typeOfBind bsQuant ++ tsArg
   in   threadBindFun config xBody tks

 | otherwise
 = thread config xBody


-- | Thread world token into a possibly functional binding.
--    If it has parameters then we know it's a function, 
--    so add the new world parameter.
threadBindFun
        :: Eq n
        => Config a n
        -> Exp a n      -- Whole expression, including lambdas.
        -> [Type n]     -- Types of function parameters.
        -> Exp a n

-- We're out of parameters. 
--  Now thread into the statements in the function body.
threadBindFun config xx []
 = threadBindStmt config xx

-- We're still decending past all the lambdas.
--  When we get to the inner-most one then add the state parameter.
threadBindFun config xx (t : tsArgs)
 = case xx of
        -- TODO: check arg type matches
        XLAM a b x
          -> XLAM a b (threadBindFun config x tsArgs)

        -- TODO: check arg type matches.
        XLam a b x      
          -> XLam a b (threadBindFun config x tsArgs)

        -- Inject a new lambda to bind the state parameter.
        _ |  Just a     <- takeAnnotOfExp xx
          ,  t == configTokenType config 
          -> XLam a (BAnon (configTokenType config))
                    (threadBindFun config xx tsArgs)

        -- Now thread into the function body.
        _ -> threadBindStmt config xx


-- | Thread world token into a binding that isn't a function.
threadBindStmt :: Eq n => Config a n -> Exp a n -> Exp a n
threadBindStmt config xx
 = case xx of
 
        XLet _ (LLet _ b x) x2
         |  Just (XVar a (UPrim nPrim _), xsArgs) 
                         <- takeXApps x
         ,  Just tNew    <- configThreadMe  config nPrim
         ,  Just mkPat   <- configThreadPat config nPrim
         -> let tWorld  = configTokenType config
                xsArgs' = xsArgs ++ [XVar a (UIx 0)]
                x'      = xApps a (XVar a (UPrim nPrim tNew)) xsArgs'
                x2'     = threadBindStmt config x2
                pat'    = mkPat (BAnon tWorld) b
            in  XCase a x' [AAlt pat' x2']

        -- A pure binding that doesn't need the token.
        XLet a lts x
         -> let x'      = threadBindStmt config x
            in  XLet a lts x'

        XCase{}         -> error "ddc-core-simpl: thread not finished"

        -- TODO: convert this to Nothing, proper exception.
        XLAM{}          -> error "ddc-core-simpl: death"
        XLam{}          -> error "ddc-core-simpl: death"
        XCast{}         -> error "ddc-core-simpl: death"
        XType{}         -> error "ddc-core-simpl: death"
        XWitness{}      -> error "ddc-core-simpl: death"

        -- For XVar, XCon, XApp as result value of function.
        _
         -> let Just a  = takeAnnotOfExp xx
                xWorld  = XVar a (UIx 0)
                wrap    = configWrapResultExp config
            in  wrap xWorld xx

{-
threadBindLets :: Eq n => Config a n -> Lets a n -> Lets a n
threadBindLets config lts
 = case lts of
        LLet m b x      
         -> let x'      = threadBindStmt config x
            in  LLet m b x'

        LRec bxs
         -> let bxs'    = [ (b, threadBindStmt config x) | (b, x) <- bxs]
            in  LRec bxs'

        LLetRegions{}   -> lts
        LWithRegion{}   -> lts
-}


-------------------------------------------------------------------------------
instance Thread Exp where
 thread config xx 
  = let down = thread config
    in case xx of
        XApp{}
         |  Just (XVar a (UPrim nPrim tPrim), xsArgs) 
                        <- takeXApps xx
         ,  Just tNew   <- configThreadMe config nPrim
         -> threadIt a nPrim tPrim tNew xsArgs

        -- Traversal boilerplate
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b (down x)
        XLam  a b x     -> XLam  a b (down x)
        XApp  a x1 x2   -> XApp  a   (down x1)  (down x2)
        XLet  a lts x2  -> XLet  a   (down lts) (down x2)
        XCase a x  alts -> XCase a   (down x)   (map down alts)
        XCast a c x     -> XCast a   c          (down x)
        XType{}         -> xx
        XWitness{}      -> xx


instance Thread Lets where
 thread config lts
  = case lts of
        LLet m b x      -> LLet m b (thread config x)  
        LRec bxs        -> LRec [(b, thread config x) | (b, x) <- bxs]
        LLetRegions{}   -> lts
        LWithRegion{}   -> lts


instance Thread Alt where
 thread config alt
  = let down = thread config
    in case alt of
        AAlt w x        -> AAlt w (down x)

-------------------------------------------------------------------------------
threadIt :: a -> n -> Type n -> Type n -> [Exp a n] -> Exp a n
threadIt a nPrim tOrig _tNew xsArgs
        = xApps a (XVar a (UPrim nPrim tOrig))
                  xsArgs
