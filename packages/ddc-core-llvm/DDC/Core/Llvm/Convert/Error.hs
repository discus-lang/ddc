
module DDC.Core.Llvm.Convert.Error
        (Error (..))
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Generic.Pretty          ()
import Data.Maybe
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Exp      as A


-- | Things that can go wrong when converting Salt to Llvm code.
--
--   As user should only hit most of these in the case of compiler errors,
--   or with hand-crafted Salt programs, most of these just an expression
--   and a simple compilaint.
--
--   Some of the other errors, like for bad type promotions and truncations,
--   can be caused by source language expessions, so we include more
--   information. We leave it to the LLVM conversion to catch these because
--   the decision of whether the conversion is valid is platform dependent.
--
--   IMPORTANT: Whether or not particular machine primitives are supported is
--   really platform dependent. Fallback implementations for unsupported
--   primitives should be implemented in a Salt-level or Source-level library,
--   and not here in the code generator.
--
data Error 
        -- Generic errors that should only be possible by compiling a
        -- hand-crafted Salt program.

        -- | Invalid Salt Bound.
        = ErrorInvalidBound
        { errorBound    :: Bound A.Name
        , errorDetails  :: Maybe String }

        -- | Invalid Salt Bind.
        | ErrorInvalidBind
        { errorBind     :: Bind A.Name
        , errorDetails  :: Maybe String }

        -- | Invalid Salt type.
        | ErrorInvalidType
        { errorType     :: Type A.Name
        , errorDetails  :: Maybe String }

        -- | Invalid Salt type constructor.
        | ErrorInvalidTyCon
        { errorTyCon    :: TyCon A.Name 
        , errorDetails  :: Maybe String }

        -- | Invalid Salt expression.
        | ErrorInvalidExp
        { errorExp      :: A.Exp
        , errorDetails  :: Maybe String }

        -- | Invalid Salt alternative.
        | ErrorInvalidAlt
        { errorAlt      :: [A.Alt]
        , errorDetails  :: Maybe String }

        -- | Invalid Super
        | ErrorInvalidSuper
        { errorBind     :: Bind A.Name
        , errorExp      :: A.Exp }

        -- | Invalid Module
        | ErrorInvalidModule
        { errorModule   :: Module () A.Name }

        -- Platform specific errors that might arise in otherwise well-typed
        -- source programs. 

        -- | The size# primitive was applied to an invalid type.
        | ErrorInvalidSizeType
        { errorType     :: Type A.Name }

        -- | The size2# primitive was applied to an invalid type.
        | ErrorInvalidSize2Type
        { errorType     :: Type A.Name }

        -- | This use of convert# is not valid on this platform.
        | ErrorInvalidConversion
        { errorTypeFrom :: Type A.Name
        , errorTypeTo   :: Type A.Name }

        -- | This use of promote# is not valid on this platform.
        | ErrorInvalidPromotion
        { errorTypeFrom :: Type A.Name
        , errorTypeTo   :: Type A.Name }

        -- | This use of truncate# is not valid on this platform.
        | ErrorInvalidTruncation
        { errorTypeFrom :: Type A.Name
        , errorTypeTo   :: Type A.Name }

        -- | Arithmetic or logic primop cannot be used at this type.
        | ErrorInvalidArith
        { errorPrimArith :: A.PrimArith
        , errorType      :: Type A.Name }
        deriving Show


instance Pretty Error where

 ppr (ErrorInvalidBound u md)
  = vcat [ text "Invalid bound: "       <> ppr u 
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidBind b md)
  = vcat [ text "Invalid bind: "        <> ppr b
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidType t md)
  = vcat [ text "Invalid type: "        <> ppr t
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidTyCon tc md)
  = vcat [ text "Invalid type constructor: " <> ppr tc
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidExp x md)
  = vcat [ text "Invalid exp: "         <> ppr x
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidAlt alts md)
  = vcat [ text "Invalid alts: "        <> ppr alts
         , text $ fromMaybe "" md ]

 ppr (ErrorInvalidSuper _n _x)
  = vcat [ text "Invalid super." ]

 ppr (ErrorInvalidModule _m)
  = vcat [ text "Invalid module." ] 

 ppr (ErrorInvalidSizeType t)
  = vcat [ text "Cannot apply size# to type '"          <> ppr t <> text "'" ]

 ppr (ErrorInvalidSize2Type t)
  = vcat [ text "Cannot apply size2# to type '"         <> ppr t <> text "'" ]

 ppr (ErrorInvalidConversion tSrc tDst)
  = vcat [ text "Cannot convert# value of type '"       <> ppr tSrc 
                <> text "' to '"                        <> ppr tDst <> text "'" ]

 ppr (ErrorInvalidPromotion tSrc tDst)
  = vcat [ text "Cannot promote# value of type '"       <> ppr tSrc 
                <> text "' to '"                        <> ppr tDst <> text "'" ]

 ppr (ErrorInvalidTruncation tSrc tDst)
  = vcat [ text "Cannot truncate# value of type '"      <> ppr tSrc 
                <> text "' to '"                        <> ppr tDst <> text "'" ]

 ppr (ErrorInvalidArith n t)
  = vcat [ text "Cannot use " <> ppr n
                <> text " at type '" <> ppr t <> text "'" ]

