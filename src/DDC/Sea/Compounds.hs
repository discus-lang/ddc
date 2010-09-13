{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Sea.Compounds
	( takeXVar
	, xInt
	, xNull)
where
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Sea.Exp


-- | Take the var from an XVar or XVarCaf
takeXVar :: Exp a -> Maybe Var
takeXVar xx
 = case xx of
	XVar name _	-> Just (varOfName name)
	_		-> Nothing


-- | Make a literal unboxed integer.
xInt :: Integer -> Exp a
xInt i	= XLit $ LLit $ LiteralFmt (LInt i) Unboxed

-- | The null pointer.
xNull :: Exp a
xNull	= XLit $ LNull