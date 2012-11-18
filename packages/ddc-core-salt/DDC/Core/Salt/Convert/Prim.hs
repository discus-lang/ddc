
-- | Pretty printers for primitive type and operator names.
module DDC.Core.Salt.Convert.Prim
        ( convPrimTyCon
        , convPrimArith2
        , convPrimStore)
where
import DDC.Core.Salt.Name
import DDC.Base.Pretty


-- | Convert a primitive type constructor to C source text.
convPrimTyCon :: PrimTyCon -> Maybe Doc
convPrimTyCon tc
 = case tc of
        PrimTyConVoid           -> Just $ text "void"
        PrimTyConBool           -> Just $ text "bool_t"
        PrimTyConNat            -> Just $ text "nat_t"
        PrimTyConInt            -> Just $ text "int_t"
        PrimTyConWord  bits     -> Just $ text "uint"    <> int bits <> text "_t"
        PrimTyConFloat bits     -> Just $ text "float"   <> int bits <> text "_t"
        PrimTyConAddr           -> Just $ text "addr_t"
        PrimTyConTag            -> Just $ text "tag_t"
        PrimTyConString         -> Just $ text "string_t"
        _                       -> Nothing


-- | Convert an binary arithmetic primop name to C source text.
convPrimArith2 :: PrimArith -> Maybe Doc
convPrimArith2 pp
 = case pp of
        -- arithmetic   
        PrimArithNeg            -> Just $ text "-"
        PrimArithAdd            -> Just $ text "+"
        PrimArithSub            -> Just $ text "-"
        PrimArithMul            -> Just $ text "*"
        PrimArithDiv            -> Just $ text "/"
        PrimArithRem            -> Just $ text "%"

        -- comparison
        PrimArithEq             -> Just $ text "=="
        PrimArithNeq            -> Just $ text "!="
        PrimArithGt             -> Just $ text ">"
        PrimArithGe             -> Just $ text ">="
        PrimArithLt             -> Just $ text "<"
        PrimArithLe             -> Just $ text "<="

        -- boolean
        PrimArithAnd            -> Just $ text "&&"
        PrimArithOr             -> Just $ text "||"

        -- bitwise
        PrimArithShl            -> Just $ text "<<"
        PrimArithShr            -> Just $ text ">>"
        PrimArithBAnd           -> Just $ text "&"
        PrimArithBOr            -> Just $ text "|"
        PrimArithBXOr           -> Just $ text "^"


-- | Convert a store primop name to C source text.
convPrimStore :: PrimStore -> Doc
convPrimStore pp
 = case pp of
        PrimStoreSize           -> text "_SIZE"
        PrimStoreSize2          -> text "_SIZE2"
        PrimStoreCreate         -> text "_CREATE"
        PrimStoreCheck          -> text "_CHECK"
        PrimStoreRecover        -> text "_RECOVER"
        PrimStoreAlloc          -> text "_ALLOC"
        PrimStoreRead           -> text "_READ"
        PrimStoreWrite          -> text "_WRITE"
        PrimStorePlusAddr       -> text "_PLUSADDR"
        PrimStoreMinusAddr      -> text "_MINUSADDr"
        PrimStorePeek           -> text "_PEEK"
        PrimStorePoke           -> text "_POKE"
        PrimStorePlusPtr        -> text "_PLUSPTR"
        PrimStoreMinusPtr       -> text "_MINUSPTR"
        PrimStoreMakePtr        -> text "_MAKEPTR"
        PrimStoreTakePtr        -> text "_TAKEPTR"
        PrimStoreCastPtr        -> text "_CASTPTR"


