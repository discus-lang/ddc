
module DDC.Type.Operators.Fixup
	(fixupKindsInType)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind
import DDC.Main.Error
import Util

stage = "DDC.Type.Fixup"

-- | When we parse a type we just give all data type constructors the kind *, which
--   will be wrong in many instances. This function looks at the kinds of the arguments
--   to give each occurrence of a data constructor a kind that will pass the kind checker.
--
--   This always works for arguments of data constructors, but types that use general
--   kind synonyms. This is really just a hack-around before we have proper kind inference.
--
fixupKindsInType :: Type -> Type
fixupKindsInType tt
 = case tt of
	TVar{}	-> tt
	TCon{}	-> tt
	
	TApp{}
	 | Just (vCon, kCon, tsArgs)	<- takeTData tt
	 , tsArgs'	<- map fixupKindsInType tsArgs
	 , ksArgs	<- map dodgyKindOfType tsArgs'
	 , kCon'	<- makeKFuns ksArgs kValue
	 -> makeTData vCon kCon' tsArgs'
	
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 , t1'		<- fixupKindsInType t1
	 , t2'		<- fixupKindsInType t2
	 -> makeTFun t1' t2' eff clo

	TForall b k t
	 -> TForall b k    $ fixupKindsInType t

	TConstrain t crs
	 -> TConstrain (fixupKindsInType t) crs
	
	_ -> panic stage $ "fixupKindsInType: no match"
	
	 
-- | Get the kind of some thing in a dodgy way, 
--   not using kindOfType.
dodgyKindOfType :: Type -> Kind
dodgyKindOfType tt
 = case tt of
	TVar k _	-> k
	TCon tc		-> tyConKind tc
	TSum k _	-> k
	
	TApp{}
	 | isJust $ takeTData tt	
	 -> kValue

	 | isJust $ takeTFun  tt
	 -> kValue
	
	TForall _ _ t	 -> dodgyKindOfType t
	TConstrain t crs -> dodgyKindOfType t
	
	_ -> panic stage $ "dodgyKindOfType: no match"
	
	
	