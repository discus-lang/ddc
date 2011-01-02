
-- | Conversion betwene boxed and unboxed versions of a type.
module DDC.Type.Boxing
	( takeUnboxedOfBoxedType
	, takeBoxedOfUnboxedType
	, tForceFn
	, tUnboxFn)
where
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Base.DataFormat
import DDC.Var
import DDC.Var.PrimId as V


-- | Take the unboxed version of a boxed type.
--   This only works for literal types, like Int32, String etc.
takeUnboxedOfBoxedType :: Type -> Maybe Type
takeUnboxedOfBoxedType tt
	| TApp (TCon tc) tR		<- tt
	, TyConData vName _ _		<- tc
	, VarIdPrim pid			<- varId vName
	, Just (_, fmtBoxed)		<- splitPrimIdWithDataFormat pid
	, dataFormatIsBoxed fmtBoxed
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmtBoxed

	-- we handle each case separately because (Ptr# String#) needs a
	-- region variable, unlike the others.
	= case pid of
		V.TBool{}	-> Just $ TCon $ tcBool   fmtUnboxed
		V.TWord{}   	-> Just $ TCon $ tcWord   fmtUnboxed
		V.TInt{}    	-> Just $ TCon $ tcInt    fmtUnboxed
		V.TFloat{}  	-> Just $ TCon $ tcFloat  fmtUnboxed
		V.TChar{}   	-> Just $ TCon $ tcChar   fmtUnboxed

		V.TString{} 	
		 |  Just tc'	<- tcString fmtUnboxed
		 -> Just $ TApp (TCon tc') tR

		_		-> Nothing
		
	| otherwise
	= Nothing
	
		
-- | Take the boxed version of an unboxed type.
--   This only works for literal types that are not String.
-- 
--   We don't handle String because that also needs a region variable.
takeBoxedOfUnboxedType :: Type -> Maybe Type
takeBoxedOfUnboxedType tt
	| TCon tc			<- tt
	, TyConData vName _ _		<- tc
	, VarIdPrim pid			<- varId vName
	, Just (_, fmtUnboxed)		<- splitPrimIdWithDataFormat pid
	, dataFormatIsUnboxed fmtUnboxed
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmtUnboxed
	= case pid of
		V.TBool{}	-> Just $ TCon $ tcBool   fmtBoxed
		V.TWord{}   	-> Just $ TCon $ tcWord   fmtBoxed
		V.TInt{}    	-> Just $ TCon $ tcInt    fmtBoxed
		V.TFloat{}  	-> Just $ TCon $ tcFloat  fmtBoxed
		V.TChar{}   	-> Just $ TCon $ tcChar   fmtBoxed
		_		-> Nothing
	
	| otherwise
	= Nothing
		
	
		
-- Builtins ---------------------------------------------------------------------------------------	
-- | Type of the forcing function for this type.
tForceFn :: Type -> Type
tForceFn tVal 	= makeTFun tVal tVal tPure tEmpty
	

-- | Type of the unboxing function for this boxed type.
--   This only works for literal types like Int32, String etc.
--  
--   The string unboxing function yields a Ptr# (String# %r1) to inside the heap object.
--   
tUnboxFn :: Type -> Maybe Type
tUnboxFn tBoxed

	-- Handle string unboxing separately.
	| TApp (TCon tcBoxed) tR	<- tBoxed
	, Just tcBoxed == tcString Boxed
	, Just (TApp (TCon tcUnboxed) _) <- takeUnboxedOfBoxedType tBoxed
	= Just 
	$ TForall  (BVar vUnboxFn_r) kRegion
	$ makeTFun	((TCon $ tcBoxed)       `TApp` tUnboxFn_r) 
			((TCon $ tcPtrU) 	`TApp` ((TCon $ tcUnboxed) `TApp` tUnboxFn_r))
			tPure
			tEmpty


	-- Other unboxing functions give the value in a register.
	| TApp (TCon tcBoxed) tR	<- tBoxed
	, Just tUnboxed			<- takeUnboxedOfBoxedType tBoxed
	= Just
	$ TForall  (BVar vUnboxFn_r) kRegion
	$ makeTFun	((TCon $ tcBoxed)	`TApp` tUnboxFn_r)
			tUnboxed
			(tRead `TApp` tR)
			tEmpty
	
	| otherwise
	= Nothing
	
	where	tUnboxFn_r	= TVar kRegion (UVar vUnboxFn_r)
		vUnboxFn_r 	= (varWithName "r")
				{ varId = VarId "tUnboxFn_r" 0, varNameSpace = NameRegion }	
	
	