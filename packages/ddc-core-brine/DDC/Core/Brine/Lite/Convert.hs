
-- | Conversion of Disciple Lite to Disciple Brine.
--
module DDC.Core.Brine.Lite.Convert
        ( toBrine
        , Error(..))
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Base.Pretty
import DDC.Type.Check.Monad                     (throw, result)
import qualified DDC.Type.Check.Monad           as G
import qualified DDC.Core.Brine.Lite.Name       as L
import qualified DDC.Core.Brine.Output.Name     as O
import qualified DDC.Core.Brine.Output.Env      as O
import qualified Data.Map                       as Map
import Control.Monad


-- | Convert a Disciple Lite module to Disciple Brine.
--
--   The input module needs to be well typed, and have all functions defined at
--   top-level, and be a-normalised. If not then `Error`.
-- 
--   Partial application and over-application are ok. These are expanded
--   to code that explicitly builds and applies thunks.
--
--   In the result, the types of algebraic data is converted to a generaic
--   "Ptr# Obj" type, and objects are explicitly constructed and destructed
--   with raw memory primops.
--
toBrine
        :: Show a 
        => Module a L.Name 
        -> Either (Error a) (Module a O.Name)
toBrine mm
 = result $ convertM mm

-- | Conversion Monad
type ConvertM a x = G.CheckM (Error a) x

data Error a
        -- | This program is definately not well typed.
        = ErrorMistyped 
        deriving Show

instance Pretty (Error a) where
 ppr err
  = case err of
        ErrorMistyped
         -> vcat [ text "Module is mistyped."]


-------------------------------------------------------------------------------
class Convert (c :: * -> *) n where
 -- | Convert a thing to the Core Brine fragment.
 --   The thing needs to be type correct, else this might `error`.
 convertM       :: c n -> ConvertM a (c O.Name)


instance Convert (Module a) L.Name where
 convertM m
  = do  x'      <- convertM $ moduleBody m

        return $ ModuleCore
         { moduleName           = moduleName m
         , moduleExportKinds    = Map.empty
         , moduleExportTypes    = Map.empty
         , moduleImportKinds    = Map.empty
         , moduleImportTypes    = Map.empty
         , moduleBody           = x' }


instance Convert (Exp a) L.Name where
 convertM xx
  = case xx of
        XVar{}  -> error "toBrineX: XVar"        
        XCon{}  -> error "toBrineX: XCon"        
        XLAM{}  -> error "toBrineX: XLAM"        
        XLam{}  -> error "toBrineX: XLam"        
        XApp{}  -> error "toBrineX: XApp"

        XLet a (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                bs'             <- mapM convertM bs
                xs'             <- mapM convertM xs
                x2'             <- convertM x2
                return $ XLet a (LRec $ zip bs' xs') x2'

        XLet{}          -> error "toBrineX: XLet"
        XCase{}         -> error "toBrineX: XCase"
        XCast{}         -> error "toBrineX: XCast"

        XType{}         -> throw $ ErrorMistyped
        XWitness{}      -> throw $ ErrorMistyped


instance Convert Bind L.Name where
 convertM bb
  = case bb of
        BNone t         -> liftM  BNone (convertM t)        
        BAnon t         -> liftM  BAnon (convertM t)
        BName n t       -> liftM2 BName (convertBindNameM n) (convertM t)


instance Convert Bound L.Name where
 convertM uu
  = case uu of
        UIx i t         -> liftM2 UIx   (return i) (convertM t)
        UName n t       -> liftM2 UName (convertBoundNameM n) (convertM t)
        UPrim n t       -> liftM2 UPrim (convertBoundNameM n) (convertM t)
        UHole   t       -> liftM  UHole (convertM t)


instance Convert Type L.Name where
 convertM tt
  = case tt of
        -- Convert type variables.
        TVar u          -> liftM TVar (convertM u)

        -- Decend into foralls, as the Brine fragment doesn't
        -- care about quantifiers.
        TForall _ t     -> convertM t

        -- Handle constructors and applications together.
        TCon{}          
         | otherwise
         -> return $ O.tPtr O.tObj       -- TODO: check for primitive types.

        TApp{}  
         -- Strip off effect and closure information.
         |  Just (t1, _, _, t2)  <- takeTFun tt
         -> liftM2 tFunPE (convertM t1) (convertM t2)

         -- Application of some type constructor or type variable, 
         -- is a standard boxed object.
         | otherwise
         -> return $ O.tPtr O.tObj

        -- We shouldn't find any TSums, as they should be thrown away by
        -- toBrineType above. We also don't call this converter on witness types.
        TSum{}          -> throw ErrorMistyped


convertBindNameM :: L.Name -> ConvertM a O.Name
convertBindNameM = error "toBrineBindName: finish me"

convertBoundNameM :: L.Name -> ConvertM a O.Name
convertBoundNameM = error "toBrineBoundName: finish me"


