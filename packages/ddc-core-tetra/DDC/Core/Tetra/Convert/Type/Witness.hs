
module DDC.Core.Tetra.Convert.Type.Witness
        ( convertCapabilityB
        , convertCapabilityT)
where
import DDC.Core.Tetra.Convert.Type.Region
import DDC.Core.Tetra.Convert.Type.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Exp.Simple
import DDC.Control.Monad.Check                  (throw)
import qualified DDC.Core.Tetra.Prim            as E
import qualified DDC.Core.Salt.Name             as A
import Control.Monad
import DDC.Data.Pretty


-- | Convert a witness binder.
convertCapabilityB :: Context -> Bind E.Name -> ConvertM a (Bind A.Name)
convertCapabilityB ctx bb
 = case bb of
        BNone t         -> liftM  BNone (convertCapabilityT ctx t)
        BAnon t         -> liftM  BAnon (convertCapabilityT ctx t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertCapabilityT ctx t)


-- | Convert a capability / coeffect type to Salt.
--   Works for Read#, Write#, Alloc#
convertCapabilityT :: Context -> Type E.Name -> ConvertM a (Type A.Name)
convertCapabilityT ctx tt
         | Just (TyConSpec tc, [tR])    <- takeTyConApps tt
         = do   tR'     <- convertRegionT ctx tR
                case tc of
                 TcConRead       -> return $ tRead  tR'
                 TcConWrite      -> return $ tWrite tR'
                 TcConAlloc      -> return $ tAlloc tR'
                 _ -> throw $ ErrorMalformed 
                            $ "Malformed capability type " ++ (renderIndent $ ppr tt)

        | otherwise
        = throw $ ErrorMalformed 
                $ "Malformed capability type " ++ (renderIndent $ ppr tt)

