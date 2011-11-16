
module DDC.Type.Check.Exp where
import DDC.Type.Exp
import DDC.Type.Check.Con


-- Check Monad ------------------------------------------------------------------------------------
data Error n
        = ErrorAppArgMismatch (Type n) (Kind n) (Kind n)
        | ErrorAppNotFun      (Type n)
        deriving Show

data CheckM n a
        = CheckM (Either (Error n) a)

instance Monad (CheckM n) where
 return x   = CheckM (Right x)
 (>>=) m f  
  = case m of
          CheckM (Left err)     -> CheckM (Left err)
          CheckM (Right x)      -> f x
          
throw :: Error n -> CheckM n a
throw err       = CheckM $ Left err


kindOfType :: Eq n => Type n -> Either (Error n) (Kind n)
kindOfType tt
 = case checkType tt of
         CheckM r       -> r

-- checkType --------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
--   TODO: add environment.
checkType :: Eq n => Type n -> CheckM n (Kind n)
checkType tt
 = case tt of
         TCon (TConType tc)
          -> return $ kindOfTyCon tc
         
         TApp t1 t2
          -> do k1      <- checkType t1
                k2      <- checkType t2
                case k1 of
                 TApp (TApp (TCon (TConKind KiConFun)) k11) k12
                  | k11 == k2   -> return k12
                  | otherwise   -> throw (ErrorAppArgMismatch tt k11 k2)
                  
                 _              -> throw (ErrorAppNotFun tt)

         TBot k
          -> return k
         