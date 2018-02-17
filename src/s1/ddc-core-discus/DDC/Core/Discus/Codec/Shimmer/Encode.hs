{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Discus.Codec.Shimmer.Encode
        (takeName)
where
import qualified DDC.Core.Discus.Prim   as D
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Op.Base       as S
import qualified Data.Text              as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
type SExp = S.Exp Text S.Prim


-- TODO: pack vars into text as may use special chars.
takeName :: D.Name -> SExp
takeName nn
 = case nn of
        D.NameVar s     -> xAps  "Dv" [xText $ T.pack s]
        D.NameCon s     -> xAps  "Dc" [xText $ T.pack s]
        D.NameExt n s   -> xAps  "De" [takeName n, xText $ T.pack s]
        _               -> xText "todo"


-- Base -------------------------------------------------------------------------------------------
-- xSym :: Text -> SExp
-- xSym tx = S.XRef (S.RSym tx)

-- xApp :: SExp -> [SExp] -> SExp
-- xApp x1 xs = S.XApp x1 xs

xAps :: Text -> [SExp] -> SExp
xAps t1 xs = S.XApp (S.XRef (S.RSym t1)) xs


-- TODO: pack this into a prim.
xText :: Text -> SExp
xText tx = S.XRef (S.RSym tx)
