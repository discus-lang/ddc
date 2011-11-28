
module DDC.Core.Check
        ( typeOfExp, typeOfExp'
        , typeOfWitness
        , typeOfWiCon
        , checkExp
        , Error(..))
where
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Base.Pretty
import DDC.Type.Check.Env               (Env)
import DDC.Type.Check.Monad             (result, throw)
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Check.Env     as Env
import qualified DDC.Type.Check.Monad   as G

type CheckM n   = G.CheckM (Error n)


-- Wrappers ---------------------------------------------------------------------------------------
-- | Take the kind of a type.
typeOfExp  :: Ord n => Exp a n p -> Either (Error n) (Type n)
typeOfExp xx 
        = result 
        $ do    (t, _eff, _clo) <- checkExpM Env.empty xx
                return t

-- | Take the kind of a type, or `error` if there isn't one.
typeOfExp' :: (Ord n, Pretty n) => Exp a n p -> Type n
typeOfExp' tt
 = case typeOfExp tt of
        Left err        -> error $ show $ ppr err
        Right k         -> k


-- | Check an expression, returning an error or its type, effect and closure.
checkExp :: Ord n => Env n -> Exp a n p -> Either (Error n) (Type n, Effect n, Closure n)
checkExp env xx = result $ checkExpM env xx


-- checkExp ---------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
--   TODO: attach kinds to bound variables, and to sums.
--         check that existing annotations have the same kinds as from the environment.
--         add a function to check that a type has kind annots in the right places.
checkExpM :: Ord n => Env n -> Exp a n p -> CheckM n (Type n, Effect n, Closure n)
checkExpM env xx
 = case xx of
        XVar _ u
         ->     return  ( typeOfBound u
                        , tBot kEffect
                        , tBot kClosure)
         
        XCon _ u
         ->     return  ( typeOfBound u
                        , tBot kEffect
                        , tBot kClosure)

        XLam _ b x
         -> do  let t1          = typeOfBind b
                _               <- checkType env t1
                (t2, eff, clo)  <- checkExpM (Env.extend b env) x
                return  ( tFun t1 t2 eff clo
                        , tBot kEffect
                        , tBot kClosure)
 
        _ -> error "typeOfExp: not handled yet"


-- checkType -------------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkType :: Ord n => Env n -> Type n -> CheckM n (Kind n)
checkType env tt
 = case T.checkType env tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

-- Witness ----------------------------------------------------------------------------------------
typeOfWitness :: Witness n -> Type n
typeOfWitness ww
 = case ww of
        WCon wc
         -> typeOfWiCon wc

        _ -> error "typeOfWitness: not handled yet"
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon -> Type n
typeOfWiCon wc
 = case wc of
        WiConPure     -> tPure  (tBot kEffect)
        WiConEmpty    -> tEmpty (tBot kClosure)

        WiConConst    
         -> tForall kRegion $ \r -> tConst r

        WiConMutable
         -> tForall kRegion $ \r -> tMutable r

        WiConLazy
         -> tForall kRegion $ \r -> tLazy r

        WiConDirect
         -> tForall kRegion $ \r -> tDirect r

        WiConRead
         -> tForall kRegion $ \r -> (tConst r) `tImpl`  (tPure  $ tRead r)

        WiConFree
         -> tForall kRegion $ \r -> (tConst r)  `tImpl` (tEmpty $ tFree r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs


-- Error ------------------------------------------------------------------------------------------
-- | Type errors.
data Error n
        -- | Found a kind error when checking a type.
        = ErrorType
        { errorTypeError        :: T.Error n }

instance (Eq n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'
        
        
