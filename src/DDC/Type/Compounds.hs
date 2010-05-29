{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Construction and destruction of common compound things.
--	Also known as 'smart' constructors and destructors.
module DDC.Type.Compounds
	( makeTFun
	, makeTFunsPureEmpty
	, makeTFunsEC
	, takeTFun)
where
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin

stage	= "DDC.Type.Compounds"

-- Function Types ---------------------------------------------------------------------------------
-- | Make a single function type
makeTFun :: Type -> Type -> Effect -> Closure -> Type
makeTFun t1 t2 eff clo
	= TApp (TApp (TApp (TApp (TCon TyConFun) t1) t2) eff) clo


-- | Make a chained function type with pure effects and empty closures
makeTFunsPureEmpty :: [Type] -> Type
makeTFunsPureEmpty []	= panic stage $ "makeTFunsPureEmpty: empty list"
makeTFunsPureEmpty xx	= makeTFunsEC tPure tEmpty xx


-- | makeTFunEC
--	Converts a list of types:	@[t1, t2, t3, t4]@
--	into a function type:		@t1 -> (t2 -> (t3 -> t4))@,
--	using the given effect and closure to annotate every function constructor.
--
--  The given list of types must be non-empty, else `panic`.
--
makeTFunsEC :: Effect -> Closure -> [Type] -> Type
makeTFunsEC _   _   (x:[])	= x
makeTFunsEC eff clo (x:xs)	= makeTFun x (makeTFunsEC eff clo xs) eff clo
makeTFunsEC _   _   []		= panic stage $ "makeTFunEC: empty list"


-- | Take a function type from a type constructor application.
takeTFun :: Type -> Maybe (Type, Type, Effect, Closure)
takeTFun tt
 	| TApp (TApp (TApp (TApp fun t1) t2) eff) clo	<- tt
	, TCon TyConFun{}	<- fun
	= Just (t1, t2, eff, clo)
	
	| otherwise
	= Nothing
