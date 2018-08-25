
module DDC.Core.Discus.Convert.Error
        (  ConvertM
        ,  Error (..))
where
import DDC.Core.Exp
import DDC.Data.Pretty
import DDC.Core.Check                                   (AnTEC(..))
import DDC.Core.Discus.Prim                              as E
import qualified DDC.Core.Discus.Transform.Curry.Error   as Curry
import qualified DDC.Control.Check                      as G

-- | Conversion Monad
type ConvertM a x = G.CheckM () (Error a) x


-- | Things that can go wrong during the conversion.
data Error a
        = ErrorCurry    Curry.Error

        -- | The 'Main' module has no 'main' function.
        | ErrorMainHasNoMain

        -- | Found unexpected AST node, like `LWithRegion`.
        | ErrorMalformed
        { errorMessage  :: String }

        -- | The program is definately not well typed.
        | ErrorMistyped
        { errorExp      :: Exp (AnTEC a E.Name) E.Name }

        -- | The program wasn't normalised, or we don't support the feature.
        | ErrorUnsupported
        { errorExp      :: Exp (AnTEC a E.Name) E.Name
        , errorDoc      :: Doc }

        | ErrorUnsupportedArg
        { errorArg      :: Arg (AnTEC a E.Name) E.Name
        , errorDoc      :: Doc }

        -- | The program has bottom (missing) type annotations.
        | ErrorBotAnnot

        -- | Found an unexpected type sum.
        | ErrorUnexpectedSum

        -- | Found an unexpected row type.
        | ErrorUnexpectedRow

        -- | Found an unbound variable.
        | ErrorUnbound
        { errorBound    :: Bound E.Name }

        -- | An invalid name used in a binding position
        | ErrorInvalidBinder
        { errorName     :: E.Name }

        -- | An invalid name used in a bound position
        | ErrorInvalidBound
        { errorBound    :: Bound E.Name }

        -- | An invalid data constructor name.
        | ErrorInvalidDaCon
        { errorDaCon    :: DaCon E.Name (Type E.Name)}

        -- | An invalid name used for the constructor of an alternative.
        | ErrorInvalidAlt
        { errorAlt      :: Alt (AnTEC a E.Name) E.Name }

        -- | Something that we can't destruct in a case expression.
        | ErrorInvalidScrut
        { errorScrut    :: Exp (AnTEC a E.Name) E.Name }

instance Show a => Pretty (Error a) where
 ppr err
  = case err of
        ErrorCurry err'
         -> ppr err'

        ErrorMalformed str
         -> vcat [ text "Module is malformed."
                 , string str ]

        ErrorMistyped xx
         -> vcat [ text "Module is mistyped." <> (string $ show xx) ]

        ErrorUnsupported xx doc
         -> vcat [ text "Cannot convert expression."
                 , indent 2 $ doc
                 , mempty
                 , indent 2 $ text "with:" %% ppr xx ]

        ErrorUnsupportedArg aa doc
         -> vcat [ text "Cannot convert argument."
                 , indent 2 $ doc
                 , mempty
                 , indent 2 $ text "with:" %% ppr aa ]

        ErrorBotAnnot
         -> vcat [ text "Found bottom type annotation."
                 , text "Program should be type-checked before conversion." ]

        ErrorUnexpectedSum
         -> vcat [ text "Unexpected type sum."]

        ErrorUnexpectedRow
         -> vcat [ text "Unexpected type row."]

        ErrorUnbound u
         -> vcat [ text "Unbound name " <> ppr u <> text "."]

        ErrorInvalidBinder n
         -> vcat [ text "Invalid name used in binder '" <> ppr n <> text "'."]

        ErrorInvalidBound n
         -> vcat [ text "Invalid name used in bound occurrence " <> ppr n <> text "."]

        ErrorInvalidDaCon n
         -> vcat [ text "Invalid data constructor name " <> ppr n <> text "." ]

        ErrorInvalidAlt alt
         -> vcat [ text "Invalid alternative."
                 , indent 2 $ text "with:" %% ppr alt ]

        ErrorInvalidScrut xx
         -> vcat [ text "Invalid scrutinee."
                 , indent 2 $ text "with:" %% ppr xx ]

        ErrorMainHasNoMain
         -> vcat [ text "Main module has no 'main' function." ]

