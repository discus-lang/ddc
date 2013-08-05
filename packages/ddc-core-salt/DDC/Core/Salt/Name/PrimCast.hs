
module DDC.Core.Salt.Name.PrimCast
        ( PrimCast (..)
        , readPrimCast
        , primCastPromoteIsValid
        , primCastTruncateIsValid)
where
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Platform
import DDC.Base.Pretty
import Control.DeepSeq


-- | Primitive cast between two types.
--
--   The exact set of available casts is determined by the target platform.
--   For example, you can only promote a @Nat\#@ to a @Word32\#@ on a 32-bit
--   system. On a 64-bit system the @Nat\#@ type is 64-bits wide, so casting it
--   to a @Word32\#@ would be a truncation.
data PrimCast
        -- | Promote a value to one of similar or larger width,
        --   without loss of precision.
        = PrimCastPromote

        -- | Truncate a value to a new width, 
        --   possibly losing precision.
        | PrimCastTruncate
        deriving (Eq, Ord, Show)

instance NFData PrimCast

instance Pretty PrimCast where
 ppr c
  = case c of
        PrimCastPromote         -> text "promote#"
        PrimCastTruncate        -> text "truncate#"


readPrimCast :: String -> Maybe PrimCast
readPrimCast str
 = case str of
        "promote#"              -> Just PrimCastPromote
        "truncate#"             -> Just PrimCastTruncate
        _                       -> Nothing


-- | Check for a valid promotion primop.
primCastPromoteIsValid 
        :: Platform     -- ^ Target platform.
        -> PrimTyCon    -- ^ Source type.
        -> PrimTyCon    -- ^ Destination type.
        -> Bool

primCastPromoteIsValid pp src dst
        -- Promote unsigned to a larger or similar width.
        | primTyConIsIntegral src, primTyConIsIntegral dst
        , primTyConIsUnsigned src, primTyConIsUnsigned dst
        , primTyConWidth pp dst >= primTyConWidth pp src
        = True

        -- Promote signed to a larger or similar width.
        | primTyConIsIntegral src, primTyConIsIntegral dst
        , primTyConIsSigned   src, primTyConIsSigned   dst
        , primTyConWidth pp dst >= primTyConWidth pp src
        = True

        -- Promote unsigned to a strictly larger unsigned width.
        | primTyConIsIntegral src, primTyConIsIntegral dst
        , primTyConIsUnsigned src, primTyConIsSigned   dst
        , primTyConWidth pp dst >  primTyConWidth pp src
        = True

        | otherwise
        = False


-- | Check for valid truncation primop.
primCastTruncateIsValid 
        :: Platform     -- ^ Target platform.
        -> PrimTyCon    -- ^ Source type.
        -> PrimTyCon    -- ^ Destination type.
        -> Bool

primCastTruncateIsValid _pp src dst
        | primTyConIsIntegral src
        , primTyConIsIntegral dst
        = True

        | otherwise
        = False
