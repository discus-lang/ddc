
-- | After type checking proper we scan through the AST to ensure that
--   all meta-type variables have been solved. If not then the type at that
--   point is ambiguous.
module DDC.Core.Check.Post
        ( checkExp )
where
import DDC.Core.Exp.Annot
import DDC.Core.Check.Error
import qualified DDC.Type.Sum   as Sum


-- | Post check an expression.
checkExp :: Exp a n -> Either (Error a n) ()
checkExp xx
 = case xx of
        XPrim{} -> return ()
        XCon{}  -> return ()
        XVar{}  -> return ()

        XAbs a p x
         -> do  checkParam a p
                checkExp x

        XApp a x1 a2
         -> do  checkExp x1
                checkArg (ErrorAmbiguousTypeExp a xx) a2

        XLet a lts x
         -> do  checkLets a lts
                checkExp  x

        XCase a x alts
         -> do  checkExp  x
                mapM_ (checkAlt a) alts

        XCast _a _c x
         ->     checkExp  x


-- | Post check a parameter.
checkParam :: a -> Param n -> Either (Error a n) ()
checkParam a pp
 = case pp of
        MType b         -> checkBind a b
        MTerm b         -> checkBind a b
        MImplicit b     -> checkBind a b


-- | Post check an argument.
checkArg :: Error a n -> Arg a n -> Either (Error a n) ()
checkArg err aa
 = case aa of
        RType t         -> checkType err t
        RTerm e         -> checkExp  e
        RWitness{}      -> return ()
        RImplicit aa'   -> checkArg  err aa'


-- | Post check a case alternative.
checkAlt :: a -> Alt a n -> Either (Error a n) ()
checkAlt a (AAlt p x)
 = do   checkPat a p
        checkExp x


-- | Post check a pattern.
checkPat :: a -> Pat n -> Either (Error a n) ()
checkPat a pp
 = case pp of
        PDefault{}      -> return ()
        PData _ bs      -> mapM_ (checkBind a) bs


-- | Post check some let bindings.
checkLets :: a -> Lets a n -> Either (Error a n) ()
checkLets a lts
 = case lts of
        LLet b x
         -> do  checkBind a b
                checkExp  x

        LRec bxs
         -> do  let (bs, xs) = unzip bxs
                mapM_ (checkBind a) bs
                mapM_ checkExp xs

        LPrivate{}
         ->     return ()


-- | Post check a binding.
checkBind :: a -> Bind n -> Either (Error a n) ()
checkBind a b
 = case b of
        BNone t         -> checkType (ErrorAmbiguousType a) t
        BAnon t         -> checkType (ErrorAmbiguousType a) t
        BName _ t       -> checkType (ErrorAmbiguousType a) t


-- | Post check a type.
checkType  :: Error a n -> Type n -> Either (Error a n) ()
checkType err tt
 = case tt of
        TCon (TyConExists _ _)
         -> Left err

        TCon _
         ->     return ()

        TVar{}
         ->     return ()

        TAbs _ t
         ->     checkType err t

        TApp t1 t2
         -> do  checkType err t1
                checkType err t2

        TForall _ t
         -> do  checkType err t

        TSum ts
         ->     mapM_ (checkType err) $ Sum.toList ts

