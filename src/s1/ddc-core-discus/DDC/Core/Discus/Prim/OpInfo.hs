
module DDC.Core.Discus.Prim.OpInfo
        ( OpInfo (..)
        , readOpInfoFlag
        , typeOpInfoFlag)
where
import DDC.Core.Discus.Prim.TyConDiscus
import DDC.Core.Discus.Prim.TyConPrim
import DDC.Core.Discus.Prim.Base
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import Control.DeepSeq


instance NFData OpInfo where
 rnf !_op = ()


instance Pretty OpInfo where
 ppr oi
  = case oi of
        OpInfoFrameNew          -> text "infoFrameNew#"
        OpInfoFramePush         -> text "infoFramePush#"
        OpInfoFrameAddData      -> text "infoFrameAddData#"
        OpInfoFrameAddSuper     -> text "infoFrameAddSuper#"


-- | Read a primitive info table operator.
readOpInfoFlag :: String -> Maybe (OpInfo, Bool)
readOpInfoFlag str
 = case str of
        "infoFrameNew#"         -> Just (OpInfoFrameNew,     False)
        "infoFrameNew##"        -> Just (OpInfoFrameNew,     True)

        "infoFramePush#"        -> Just (OpInfoFramePush,    False)
        "infoFramePush##"       -> Just (OpInfoFramePush,    True)

        "infoFrameAddData#"     -> Just (OpInfoFrameAddData, False)
        "infoFrameAddData##"    -> Just (OpInfoFrameAddData, True)

        "infoFrameAddSuper#"    -> Just (OpInfoFrameAddSuper, False)
        "infoFrameAddSuper##"   -> Just (OpInfoFrameAddSuper, True)

        _                       -> Nothing


-- | Take the type of a primitive info table operator.
typeOpInfoFlag :: OpInfo -> Bool -> Type Name
typeOpInfoFlag op False
 = case op of
        OpInfoFrameNew
         -> tNat  `tFun` tAddr

        OpInfoFramePush
         -> tAddr `tFun` tUnit

        OpInfoFrameAddData
         -> tAddr `tFun` tWord 16 `tFun` tWord 16
                  `tFun` tTextLit `tFun` tTextLit
                  `tFun` tWord 32

        OpInfoFrameAddSuper
         -> tAddr `tFun` tWord 16 `tFun` tWord 16
                  `tFun` tTextLit `tFun` tTextLit
                  `tFun` tWord 32

typeOpInfoFlag op True
 = case op of
        OpInfoFrameNew
         -> tUnboxed tNat  `tFun` tUnboxed tAddr

        OpInfoFramePush
         -> tUnboxed tAddr `tFun` tUnit

        OpInfoFrameAddData
         -> tUnboxed tAddr `tFun` tUnboxed (tWord 16) `tFun` tUnboxed (tWord 16)
                           `tFun` tUnboxed tTextLit   `tFun` tUnboxed tTextLit
                           `tFun` tUnboxed (tWord 32)

        OpInfoFrameAddSuper
         -> tUnboxed tAddr `tFun` tUnboxed (tWord 16) `tFun` tUnboxed (tWord 16)
                           `tFun` tUnboxed tTextLit   `tFun` tUnboxed tTextLit
                           `tFun` tUnboxed (tWord 32)
