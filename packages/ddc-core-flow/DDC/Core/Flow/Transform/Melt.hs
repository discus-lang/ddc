
module DDC.Core.Flow.Transform.Melt
        ( Info (..)
        , meltModule )
where
import DDC.Core.Flow.Prim
import DDC.Core.Module
import DDC.Core.Exp
import Control.Monad.Writer.Strict
import qualified Data.Set               as Set
import Data.Set                         (Set)

-------------------------------------------------------------------------------
-- | Contains binders of variables that have been melted.
data Info
        = Info (Set Name)

instance Monoid Info where
 mempty                         = Info (Set.empty)
 mappend (Info s1) (Info s2)    = Info (Set.union s1 s2)


-------------------------------------------------------------------------------
-- | Melt compound data structures in a module.
meltModule :: Module a Name -> (Module a Name, Info)
meltModule mm
 = let  (xBody', info)  = runWriter (melt $ moduleBody mm)
   in   (mm { moduleBody = xBody' }, info)


-------------------------------------------------------------------------------
class Melt c where
 melt :: c -> Writer Info c

instance Melt (Exp a Name) where
 melt xx
  = case xx of
        XVar  a u       -> return $ XVar a u
        XCon  a dc      -> return $ XCon a dc
        XLAM  a b x     -> liftM  (XLAM a b)  (melt x)
        XLam  a b x     -> liftM  (XLam a b)  (melt x)
        XApp  a x1 x2   -> liftM2 (XApp a)    (melt x1)  (melt x2)
        XLet  a lts x   -> liftM2 (XLet a)    (melt lts) (melt x)
        XCase a x alts  -> liftM2 (XCase a)   (melt x)   (mapM melt alts)
        XCast a c x     -> liftM  (XCast a c) (melt x)
        XType t         -> return $ XType t
        XWitness w      -> return $ XWitness w


instance Melt (Lets a Name) where
 melt lts
  = case lts of
        LLet b x        -> liftM (LLet b) (melt x)

        LRec bxs        
         -> do  let (bs, xs) = unzip bxs
                xs'      <- mapM melt xs
                return   $  LRec $ zip bs xs'

        LLetRegions{}   -> return lts
        LWithRegion{}   -> return lts


instance Melt (Alt a Name) where
 melt (AAlt w x)        = liftM (AAlt w) (melt x)

