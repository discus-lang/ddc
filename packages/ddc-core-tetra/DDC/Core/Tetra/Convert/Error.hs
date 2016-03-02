
module DDC.Core.Tetra.Convert.Error
        (  ConvertM
        ,  Error (..))
where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Check                           (AnTEC(..))
import DDC.Core.Tetra.Prim                      as E
import qualified DDC.Control.Monad.Check        as G


-- | Conversion Monad
type ConvertM a x = G.CheckM () (Error a) x


-- | Things that can go wrong during the conversion.
data Error a
        -- | The 'Main' module has no 'main' function.
        = ErrorMainHasNoMain

        -- | Found unexpected AST node, like `LWithRegion`.
        | ErrorMalformed String

        -- | The program is definately not well typed.
        | ErrorMistyped  (Exp (AnTEC a E.Name) E.Name)

        -- | The program wasn't normalised, or we don't support the feature.
        | ErrorUnsupported (Exp (AnTEC a E.Name) E.Name) Doc

        -- | The program has bottom (missing) type annotations.
        | ErrorBotAnnot

        -- | Found an unexpected type sum.
        | ErrorUnexpectedSum

        -- | Found an unbound variable.
        | ErrorUnbound      (Bound E.Name)

        -- | An invalid name used in a binding position
        | ErrorInvalidBinder E.Name

        -- | An invalid name used in a bound position
        | ErrorInvalidBound (Bound E.Name)

        -- | An invalid data constructor name.
        | ErrorInvalidDaCon (DaCon E.Name)

        -- | An invalid name used for the constructor of an alternative.
        | ErrorInvalidAlt

        -- | Super is not fully named.
        | ErrorSuperUnnamed   (Bind Name)

        -- | Super is not in prenex form.
        | ErrorSuperNotPrenex (Bind Name)


instance Show a => Pretty (Error a) where
 ppr err
  = case err of
        ErrorMalformed str
         -> vcat [ text "Module is malformed."
                 , text str ]

        ErrorMistyped xx
         -> vcat [ text "Module is mistyped."           <> (text $ show xx) ]

        ErrorUnsupported xx doc
         -> vcat [ text "Cannot convert expression."
                 , indent 2 $ doc
                 , empty
                 , indent 2 $ text "with:" <+> ppr xx ]

        ErrorBotAnnot
         -> vcat [ text "Found bottom type annotation."
                 , text "Program should be type-checked before conversion." ]

        ErrorUnexpectedSum
         -> vcat [ text "Unexpected type sum."]

        ErrorUnbound u
         -> vcat [ text "Unbound name " <> ppr u <> text "."]

        ErrorInvalidBinder n
         -> vcat [ text "Invalid name used in binder '" <> ppr n <> text "'."]

        ErrorInvalidBound n
         -> vcat [ text "Invalid name used in bound occurrence " <> ppr n <> text "."]

        ErrorInvalidDaCon n
         -> vcat [ text "Invalid data constructor name " <> ppr n <> text "." ]

        ErrorInvalidAlt
         -> vcat [ text "Invalid alternative." ]

        ErrorMainHasNoMain
         -> vcat [ text "Main module has no 'main' function." ]


        ErrorSuperUnnamed b
         -> vcat [ text "Super with binder " 
                        <> (squotes $ ppr b) <> text " lacks a name." ]

        ErrorSuperNotPrenex b
         -> vcat [ text "Super " 
                        <> (squotes $ ppr b) <> text " is not in prenex form." ]
