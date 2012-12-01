
-- | Generating names for bindings created by the projection desugarer.
module DDC.Desugar.Projections.Naming
	( newProjFunVar
	, newInstFunVar
	, makeTypeName)
where
import DDC.Desugar.Projections.Base
import DDC.Type
import DDC.Var
import Source.Desugar			(spOfAnnot)
import qualified Shared.VarUtil		as Var
import Util

-- | Create a name for a top level projection function.
--	Add the type and projection names to the var to make the core IR readable.
newProjFunVar :: Annot -> ModuleId -> Var -> Var -> ProjectM Var
newProjFunVar annot modName@(ModuleId ms) vCon vField
 = do 	var	<- newVarN NameValue
	return	$ var
		{ varName 	= Var.deSymString
				$ "project_"
	 			++ varName vCon 	++ "_"
				++ varName vField

		, varInfo 	= [ISourcePos (spOfAnnot annot) ]
		, varModuleId	= modName }


-- | Create a name for a top level type class instance function
--	Add the type class and function names to the var to make the core IR readable.
newInstFunVar :: Annot -> ModuleId -> Var -> [Type] -> Var -> ProjectM Var
newInstFunVar annot modName@(ModuleId ms) vClass tsArgs vInst
 = do 	var	<- newVarN NameValue
	return	$ var
		{ varName	= Var.deSymString
				$ "instance_"
				++ varName vClass	 	++ "_"
				++ catMap makeTypeName tsArgs	++ "_"
				++ varName vInst

		, varInfo 	= [ISourcePos (spOfAnnot annot) ]
		, varModuleId 	= modName }


-- | Make a printable name from a type
--   TODO: do this more intelligently, in a way guaranteed not to clash with other types
makeTypeName :: Type -> String
makeTypeName tt
 = case tt of
	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> "Fun" ++ makeTypeName t1 ++ makeTypeName t2

	 | Just (v, _, ts)		<- takeTData tt
	 -> varName v ++ catMap makeTypeName ts

	TCon{}
	 | Just (v, _, ts)		<- takeTData tt
	 -> varName v ++ catMap makeTypeName ts

	TVar k v
	 -> ""

