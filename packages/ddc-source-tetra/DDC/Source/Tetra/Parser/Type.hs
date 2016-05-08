{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Parser.Type
        ( pTyCon
        , pTyConBound)
where
import DDC.Source.Tetra.Exp.Source      as S
import DDC.Source.Tetra.Prim.TyConTetra as S
import DDC.Core.Tetra                   as C
import DDC.Core.Lexer.Tokens            as C
import DDC.Core.Parser                  (Parser)
import DDC.Base.Parser                  ((<?>))
import Data.Text                        (Text)
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as T


-- | Parse a type constructor.
pTyCon :: Parser Text TyCon
pTyCon  =   P.pTokMaybe f 
        <?> "a type constructor"

 where f kk
        = case kk of
                -- Primitive Ambient TyCons.
                KA (KSoConBuiltin c)    
                 -> Just (TyConPrim $ TyConPrimSoCon c)

                KA (KKiConBuiltin c)    
                 -> Just (TyConPrim $ TyConPrimKiCon c)

                KA (KTwConBuiltin c)
                 -> Just (TyConPrim $ TyConPrimTwCon c)

                KA (KTcConBuiltin c)
                 -> Just (TyConPrim $ TyConPrimTcCon c)

                -- Primitive Machine TyCons.
                KN (KCon tx)
                 |  Just tc  <- C.readPrimTyCon (T.unpack tx)
                 -> Just $ TyConPrim $ TyConPrimMach  tc

                -- Primitive Tetra TyCons.
                KN (KCon tx)
                 |  Just tc  <- S.readPrimTyConTetra (T.unpack tx)
                 -> Just $ TyConPrim $ TyConPrimTetra tc

                -- User Bound TyCons.
                KN (KCon tx)
                 -> Just (TyConBound tx)

                _ -> Nothing


-- | Parse a bound type constructor.
--   Known primitive type constructors do not match.
pTyConBound :: Parser Text TyCon
pTyConBound  
        =   P.pTokMaybe f
        <?> "a bound type constructor"
 where  
        f :: Tok Text -> Maybe TyCon
        f (KN (KCon tx))          
         |  Nothing <- C.readPrimTyCon      (T.unpack tx)
         ,  Nothing <- S.readPrimTyConTetra (T.unpack tx)
         = Just (TyConBound tx)

        f _ = Nothing


