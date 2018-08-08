
module DDC.Core.Discus.Convert.Exp.Lit
        (convertLitCtor)
where
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import qualified DDC.Core.Discus.Prim     as E
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A
import DDC.Control.Check                (throw)


-- | Convert a literal constructor to Salt.
--   These are values that have boxable index types like Bool# and Nat#.
convertLitCtor
        :: a                            -- ^ Annot from deconstructed XCon node.
        -> DaCon E.Name (Type E.Name)   -- ^ Data constructor of literal.
        -> ConvertM a (Exp a A.Name)

convertLitCtor a dc
 | Just (E.NameLitUnboxed n)    <- takeNameOfDaConPrim dc
 = case n of
        E.NameLitBool    b      -> return $ A.xBool    a b
        E.NameLitNat     i      -> return $ A.xNat     a i
        E.NameLitInt     i      -> return $ A.xInt     a i
        E.NameLitSize    i      -> return $ A.xSize    a i
        E.NameLitWord    i bits -> return $ A.xWord    a i bits
        E.NameLitFloat   f bits -> return $ A.xFloat   a f bits
        E.NameLitChar    c      -> return $ A.xChar    a c
        E.NameLitTextLit bs     -> return $ A.xTextLit a bs
        _                       -> throw  $ ErrorMalformed "Invalid literal."

 | otherwise
 = throw $ ErrorMalformed "Invalid literal."

