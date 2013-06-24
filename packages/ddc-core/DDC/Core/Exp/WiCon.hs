
module DDC.Core.Exp.WiCon
        ( WiCon  (..)
        , WbCon  (..))
where
import DDC.Type.Exp
import DDC.Type.Sum     ()
import Control.DeepSeq


-- | Witness constructors.
data WiCon n
        -- | Witness constructors baked into the language.
        = WiConBuiltin !WbCon

        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        | WiConBound   !(Bound n) !(Type n)
        deriving (Show, Eq)


-- | Built-in witness constructors.
--
--   These are used to convert a runtime capability into a witness that
--   the corresponding property is true.
data WbCon
        -- | (axiom) The pure effect is pure.
        -- 
        --   @pure     :: Pure !0@
        = WbConPure 

        -- | (axiom) The empty closure is empty.
        --
        --   @empty    :: Empty $0@
        | WbConEmpty

        -- | Convert a capability guaranteeing that a region is in the global
        --   heap, into a witness that a closure using this region is empty.
        --   This lets us rely on the garbage collector to reclaim objects
        --   in the region. It is needed when we suspend the evaluation of 
        --   expressions that have a region in their closure, because the
        --   type of the returned thunk may not reveal that it references
        --   objects in that region.
        -- 
        --  @use      :: [r : %]. Global r => Empty (Use r)@
        | WbConUse      

        -- | Convert a capability guaranteeing the constancy of a region, into
        --   a witness that a read from that region is pure.
        --   This lets us suspend applications that read constant objects,
        --   because it doesn't matter if the read is delayed, we'll always
        --   get the same result.
        --
        --   @read     :: [r : %]. Const r  => Pure (Read r)@
        | WbConRead     

        -- | Convert a capability guaranteeing the constancy of a region, into
        --   a witness that allocation into that region is pure.
        --   This lets us increase the sharing of constant objects,
        --   because we can't tell constant objects of the same value apart.
        -- 
        --  @alloc    :: [r : %]. Const r  => Pure (Alloc r)@
        | WbConAlloc
        deriving (Show, Eq)


-- NFData ---------------------------------------------------------------------
instance NFData n => NFData (WiCon n) where
 rnf wi
  = case wi of
        WiConBuiltin wb         -> rnf wb
        WiConBound   u t        -> rnf u `seq` rnf t

instance NFData WbCon
