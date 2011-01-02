
-- | Conversion of literal values.
module DDC.Desugar.ToCore.Literal
	(toCoreXLit)
where
import DDC.Desugar.ToCore.Base
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.Literal
import DDC.Base.DataFormat
import qualified DDC.Type			as T
import qualified DDC.Desugar.Exp		as D
import qualified DDC.Core.Exp			as C

stage		= "DDC.Desugar.ToCore.Literal"

-- | Convert a literal to core
--	The desugared language supports boxed literals, but all literals in the core
--	should be unboxed.
toCoreXLit :: T.Type -> D.Exp Annot -> C.Exp
toCoreXLit tt xLit
 	= toCoreXLit' (T.stripToBodyT tt) xLit

toCoreXLit' tt xLit@(D.XLit _ litfmt@(LiteralFmt lit fmt))

	-- raw unboxed strings need their region applied
	| LString _	<- lit
	, Unboxed	<- fmt
	= let	Just (_, _, [tR]) = T.takeTData tt
	  in	C.XAPP (C.XLit litfmt) tR

	-- other unboxed literals have kind *, so there is nothing more to do
	| dataFormatIsUnboxed fmt
	= C.XLit litfmt
	
	-- unboxed strings have kind % -> *, 
	--	so we need to apply the region to the unboxed literal
	--	when building the boxed version.
	| LString _	<- lit
	, Boxed		<- fmt
	= let	Just (_, _, [tR]) = T.takeTData tt

		Just fmtUnboxed	  = dataFormatUnboxedOfBoxed fmt
		tBoxed		  = tt
		Just tUnboxed	  = error ("Literal.hs: tUnboxed")
		tFun		  = T.makeTFun tUnboxed tBoxed T.tPure T.tEmpty

	  in	C.XApp	(C.XPrim C.MBox tFun) 
			(C.XAPP  (C.XLit $ LiteralFmt lit fmtUnboxed) tR)

	-- the other unboxed literals have kind *, 
	--	so we can just pass them to the the boxing primitive directly.
	| Just (_, _, [tR]) <- T.takeTData tt
	= let	Just fmtUnboxed	= dataFormatUnboxedOfBoxed fmt
		tBoxed		= tt
		tUnboxed	= error ("Literal.hs: tUnboxed")
		tFun		= T.makeTFun tUnboxed tBoxed T.tPure T.tEmpty
		
	  in	C.XApp	(C.XPrim C.MBox tFun)
			(C.XLit $ LiteralFmt lit fmtUnboxed)
				
	| otherwise
	= panic stage
		$ "toCoreLitX: no match\n"
		% "   tLit   = " % show tt	% "\n"
		% "   xLit   = " % show xLit	% "\n"

toCoreXLit' _ _
	= panic stage
	$ "toCoreXLit: not an XLit"
