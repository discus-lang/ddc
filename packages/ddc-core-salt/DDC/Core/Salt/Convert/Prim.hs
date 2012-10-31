
-- | Pretty printers for primitive type and operator names.
module DDC.Core.Salt.Convert.Prim
        ( convPrimTyCon
        , convPrimOp2
        , convPrimStore
        , convPrimExternal)
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
convPrimOp2 :: PrimOp -> Maybe Doc
convPrimOp2 pp
 = case pp of
        -- arithmetic   
        PrimOpNeg               -> Just $ text "-"
        PrimOpAdd               -> Just $ text "+"
        PrimOpSub               -> Just $ text "-"
        PrimOpMul               -> Just $ text "*"
        PrimOpDiv               -> Just $ text "/"
        PrimOpRem               -> Just $ text "%"

        -- comparison
        PrimOpEq                -> Just $ text "=="
        PrimOpNeq               -> Just $ text "!="
        PrimOpGt                -> Just $ text ">"
        PrimOpGe                -> Just $ text ">="
        PrimOpLt                -> Just $ text "<"
        PrimOpLe                -> Just $ text "<="

        -- boolean
        PrimOpAnd               -> Just $ text "&&"
        PrimOpOr                -> Just $ text "||"

        -- bitwise
        PrimOpShl               -> Just $ text "<<"
        PrimOpShr               -> Just $ text ">>"
        PrimOpBAnd              -> Just $ text "&"
        PrimOpBOr               -> Just $ text "|"
        PrimOpBXOr              -> Just $ text "^"


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


-- | Convert an external primop name to C source text.
convPrimExternal :: PrimExternal -> Doc
convPrimExternal pp
 = case pp of
        PrimExternalShowInt     -> text "_showInt"
        PrimExternalPutStr      -> text "_putStr"
        PrimExternalPutStrLn    -> text "_putStrLn"


