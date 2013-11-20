
module DDC.Core.Flow.Transform.Melt
        ( Info (..)
        , meltModule )
where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Compounds
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
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
meltModule :: Module () Name -> (Module () Name, Info)
meltModule mm
 = let  (xBody', info)  
                = runWriter 
                $ melt 
                $ deannotate (const Nothing) $ moduleBody mm

   in   (mm { moduleBody = annotate () xBody' }, info)


-------------------------------------------------------------------------------
class Melt c where
 melt :: c -> Writer Info c


-- Exp ------------------------------------------------------------------------
instance Melt (Exp () Name) where

 -- Melt allocations of tuple references.
 --
 --    let b    = new [TupleN# tA1 tA2] xInit  in  ...
 --
 -- => let b$1  = new [tA1] (projN_1 xInit)    in
 --    let b$2  = new [tA2] (projN_2 xInit)    in  ...
 --
 melt (XLet (LLet b x1) x2)
  | BName nBind _t                             <- b
  , Just ( NameOpStore OpStoreNew
         , [XType tElem, xInit])               <- takeXPrimApps x1
  , Just ( NameTyConFlow (TyConFlowTuple n)
         , tAs)                                <- takePrimTyConApps tElem
  , length tAs == n
  = do  
        let ltsNew 
                = [ LLet (BName (NameVarMod nBind (show i)) (tRef tA))
                    $ xNew  tA (xProj tAs i xInit)
                        | i     <- [1..n]
                        | tA    <- tAs ]

        x2'      <- melt x2
        return  $ xLets ltsNew x2' 


 -- Melt reads from tuple references.
 --
 --    let b     = read# [TupleN# tA1 tA2] xR  in ...
 --
 -- => let b.1   = read# [tA1] xRef$1 in
 --    let b.2   = read# [tA2] xRef$2 in
 --    let b     = TN# [tA1] [tA2] b$1 b$2 in ...
 --
 melt (XLet (LLet b x1) x2)
  | BName nBind _t                              <- b
  , Just ( NameOpStore OpStoreRead
         , [XType tElem, XVar (UName nRef)])    <- takeXPrimApps x1
  , Just ( NameTyConFlow (TyConFlowTuple n)
         , tsA)                                 <- takePrimTyConApps tElem
  , length tsA == n
  = do  
        -- read all the components
        let ltsRead 
                = [LLet (BName (NameVarMod nBind (show i)) tA)
                    $ xRead tA
                        (XVar (UName (NameVarMod nRef (show i))))
                        | i     <- [1..n]
                        | tA    <- tsA ]

        -- build the result tuple
        let ltOrig      
                = LLet b 
                $ xApps (XCon (dcTupleN n))
                        (   [XType t    | t <- tsA] 
                         ++ [XVar (UName (NameVarMod nBind (show i)))
                                        | i <- [1..n]])

        -- melt the body
        x2'     <- melt x2

        return  $ xLets (ltsRead ++ [ltOrig]) x2'


 -- Melt writes to tuple references.
 --
 --    let _ = write# [TupleN# tA1 tA2] xR xV in ...
 --
 -- => let _ = write# [tA1] xR$1 (projN_1 xV) 
 --    let _ = write# [tA2] xR$2 (projN_2 xV) in ...
 --
 melt (XLet (LLet b x1) x2)
  | BNone tB                                     <- b
  , Just ( NameOpStore OpStoreWrite 
         , [XType tElem, XVar (UName nRef), xV]) <- takeXPrimApps x1
  , Just ( NameTyConFlow (TyConFlowTuple n)
         , tsA)                                  <- takePrimTyConApps tElem
  , length tsA == n
  = do  
        let ltsWrite
                = [ LLet (BNone tB)
                    $ xWrite tA
                        (XVar (UName (NameVarMod nRef (show i))))
                        (xProj tsA i xV)
                        | i     <- [1..n]
                        | tA    <- tsA ]

        x2'     <- melt x2
        return  $ xLets ltsWrite x2'


 -- Boilerplate
 melt xx
  = case xx of
        XAnnot a x      -> liftM  (XAnnot a) (melt x)
        XLet  lts x     -> liftM2 XLet       (melt lts) (melt x)
        XApp  x1 x2     -> liftM2 XApp       (melt x1)  (melt x2)
        XVar  u         -> return $ XVar u
        XCon  dc        -> return $ XCon dc
        XLAM  b x       -> liftM  (XLAM b)   (melt x)
        XLam  b x       -> liftM  (XLam b)   (melt x)
        XCase x alts    -> liftM2 XCase      (melt x)   (mapM melt alts)
        XCast c x       -> liftM  (XCast c)  (melt x)
        XType t         -> return $ XType t
        XWitness w      -> return $ XWitness w


-- Lets -----------------------------------------------------------------------
instance Melt (Lets () Name) where
 melt lts
  = case lts of
        LLet b x        -> liftM (LLet b) (melt x)
        LRec bxs        
         -> do  let (bs, xs) = unzip bxs
                xs'      <- mapM melt xs
                return   $  LRec $ zip bs xs'

        LPrivate{}      -> return lts
        LWithRegion{}   -> return lts


-- Alt ------------------------------------------------------------------------
instance Melt (Alt () Name) where
 melt (AAlt w x)        = liftM (AAlt w) (melt x)

