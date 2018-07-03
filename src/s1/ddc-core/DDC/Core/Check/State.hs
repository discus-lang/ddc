
module DDC.Core.Check.State
        ( CheckM, G.MonadIO(..)
        , G.throw, G.runCheck, G.evalCheck, G.mapErr
        , newExists, newPos

        , CheckTrace(..)
        , emptyCheckTrace
        , unionCheckTrace
        , ctrace)
where
import qualified DDC.Control.CheckIO    as G
import DDC.Type.Exp
import DDC.Core.Check.Error
import DDC.Core.Check.Context.Elem
import DDC.Data.Pretty                  as P
import qualified Data.Semigroup         as SG


-- | Type checker monad.
--   Used to manage type errors.
type CheckM a n
        = G.CheckM (CheckTrace, Int, Int) (Error a n)


-- | Allocate a new existential.
newExists :: Kind n -> CheckM a n (Exists n)
newExists k
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix + 1, pos)
        return  (Exists ix k)


-- | Allocate a new context stack position.
newPos :: CheckM a n Pos
newPos
 = do   (tr, ix, pos)       <- G.get
        G.put (tr, ix, pos + 1)
        return  (Pos pos)


-- CheckTrace -----------------------------------------------------------------
-- | Human readable trace of the type checker.
data CheckTrace
        = CheckTrace
        { checkTraceDoc :: Doc }

instance Pretty CheckTrace where
 ppr ct         = checkTraceDoc ct

instance SG.Semigroup CheckTrace where
 (<>)           = unionCheckTrace

instance Monoid CheckTrace where
 mempty         = emptyCheckTrace
 mappend        = unionCheckTrace


-- | Construct an empty `CheckTrace`.
emptyCheckTrace :: CheckTrace
emptyCheckTrace = CheckTrace mempty


-- | Union two `ChecTrace`s.
unionCheckTrace :: CheckTrace -> CheckTrace -> CheckTrace
unionCheckTrace ct1 ct2
        = CheckTrace
        { checkTraceDoc = checkTraceDoc ct1 <> checkTraceDoc ct2 }


-- | Append a doc to the checker trace.
ctrace :: Doc -> CheckM a n ()
ctrace doc'
 = do   (tr, ix, pos)       <- G.get
        let tr' = tr { checkTraceDoc = checkTraceDoc tr `P.pline` doc' }
        G.put (tr', ix, pos)
        return  ()

