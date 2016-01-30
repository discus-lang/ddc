
module DDC.Core.Tetra.Convert.Type.Kind
        ( convertTypeB
        , convertTypeU
        , convertK)
where
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw)
import Control.Monad
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Name             as A


-- | Convert a type binder.
--   These are formal type parameters.
convertTypeB    :: Bind E.Name -> ConvertM a (Bind A.Name)
convertTypeB bb
 = case bb of
        BNone k         -> liftM BNone  (convertK k)
        BAnon k         -> liftM BAnon  (convertK k)
        BName n k       -> liftM2 BName (convertBindNameM n) (convertK k)


-- | Convert a type bound.
--   These are bound by formal type parametrs.
convertTypeU    :: Bound E.Name -> ConvertM a (Bound A.Name)
convertTypeU uu
 = case uu of
        UIx i                   
         -> return $ UIx i

        UName (E.NameVar str)   
         -> return $ UName (A.NameVar str)

        UPrim (E.NameVar str) k
         -> do  k'      <- convertK k
                return $ UPrim (A.NameVar str) k'

        _ -> throw $ ErrorInvalidBound uu


-- | Convert a kind from Core Tetra to Core Salt.
convertK :: Kind E.Name -> ConvertM a (Kind A.Name)
convertK kk
        | TCon (TyConKind kc) <- kk
        = return $ TCon (TyConKind kc)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Invalid kind " ++ (renderIndent $ ppr kk)
