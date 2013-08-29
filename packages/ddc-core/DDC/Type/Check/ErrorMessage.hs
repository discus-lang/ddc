
-- | Errors produced when checking types.
module DDC.Type.Check.ErrorMessage
where
import DDC.Type.Check.Error
import DDC.Type.Compounds
import DDC.Type.Pretty


instance (Eq n, Show n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorUndefined u
         -> text "Undefined type variable: " <> ppr u

        ErrorUndefinedTypeCtor u
         -> text "Undefined type constructor: " <> ppr u

        ErrorUnappliedKindFun 
         -> text "Can't take sort of unapplied kind function constructor."
        
        ErrorNakedSort s
         -> text "Can't check a naked sort: " <> ppr s
        
        ErrorVarAnnotMismatch u t
         -> vcat [ text "Type mismatch in annotation."
                 , text "             Variable: "       <> ppr u
                 , text "       has annotation: "       <> ppr u
                 , text " which conflicts with: "       <> ppr t
                 , text "     from environment." ]
 
        ErrorAppArgMismatch tt t1 t2
         -> vcat [ text "Core type mismatch in application."
                 , text "             type: " <> ppr t1
                 , text "   does not match: " <> ppr t2
                 , text "   in application: " <> ppr tt ]
         
        ErrorAppNotFun tt t1 k1 t2 k2
         -> vcat [ text "Core type mismatch in application."
                 , text "     cannot apply type: " <> ppr t2
                 , text "               of kind: " <> ppr k2
                 , text "  to non-function type: " <> ppr t1
                 , text "               of kind: " <> ppr k1
                 , text "         in appliction: " <> ppr tt]
                
        ErrorSumKindMismatch k ts ks
         -> vcat 
              $  [ text "Core type mismatch in sum."
                 , text " found multiple types: " <> ppr ts
                 , text " with differing kinds: " <> ppr ks ]
                 ++ (if k /= tBot sComp
                        then [text "        expected kind: " <> ppr k ]
                        else [])
                
        ErrorSumKindInvalid ts k
         -> vcat [ text "Invalid kind for type sum."
                 , text "         the type sum: " <> ppr ts
                 , text "             has kind: " <> ppr k
                 , text "  but it must be ! or $" ]

        ErrorForallKindInvalid tt t k
         -> vcat [ text "Invalid kind for body of quantified type."
                 , text "        the body type: " <> ppr t
                 , text "             has kind: " <> ppr k
                 , text "  but it must be * or @" 
                 , text "        when checking: " <> ppr tt ]
        
        ErrorWitnessImplInvalid tt t1 k1 t2 k2
         -> vcat [ text "Invalid args for witness implication."
                 , text "            left type: " <> ppr t1
                 , text "             has kind: " <> ppr k1
                 , text "           right type: " <> ppr t2
                 , text "             has kind: " <> ppr k2 
                 , text "        when checking: " <> ppr tt ]
                
