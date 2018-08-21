
module DDC.Core.Exp.Simple
        ( type Exp

        -- * Exp Constructors
        , pattern XPrm,  pattern XCon,  pattern XVar
        , pattern XAbs,  pattern XApp
        , pattern XLet
        , pattern XCase, pattern XCast

        -- * Exp Sugar
        , pattern XLam,  pattern XLAM
        , pattern XLLet, pattern XLRec, pattern XLPrivate
        , pattern XBox,  pattern XRun

        , A.ParamSort (..)
        , A.ParamMode (..)
        , A.Param
        , Arg)
where
import qualified DDC.Core.Exp.Annot     as A


-- TODO: add type syntax.
-- TODO: replace generic version of AST used in LLVM gen with this approach.

-- Types
type Exp n                      = A.Exp () n

-- Exp Constructors
pattern XPrm p                  = A.XPrim () p
pattern XCon  c                 = A.XCon  () c
pattern XVar  b                 = A.XVar  () b
pattern XAbs  p x               = A.XAbs  () p x
pattern XApp  x1 x2             = A.XApp  () x1 x2
pattern XLet  lts x             = A.XLet  () lts x
pattern XCase x alts            = A.XCase () x alts
pattern XCast c x               = A.XCast () c x

-- Exp Sugar
pattern XLam   b x              = A.XAbs  () (A.MTerm b) x
pattern XLAM   b x              = A.XAbs  () (A.MType b) x
pattern XLLet  b x1 x2          = A.XLet  () (A.LLet b x1) x2
pattern XLRec  bxs x2           = A.XLet  () (A.LRec bxs) x2
pattern XLPrivate bs mt ws x2   = A.XLet  () (A.LPrivate bs mt ws) x2
pattern XBox   x                = A.XCast () A.CastBox x
pattern XRun   x                = A.XCast () A.CastRun x

-- Arguments
type Arg n                      = A.Arg   () n