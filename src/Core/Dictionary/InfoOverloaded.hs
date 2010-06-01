
module Core.Dictionary.InfoOverloaded
	( InfoOverloaded(..)
	, lookupOverloadedVar )
where
import Core.Dictionary.Env
import Core.Exp
import Core.Glob
import Type.Util.Bits
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Util
import Data.Sequence		(Seq)
import qualified Data.Sequence	as Seq
import qualified Data.Map	as Map

stage 	= "Core.Dictionary.InfoOverloaded"

-- | Holds information about an overloaded variable.
data InfoOverloaded
	= InfoOverloaded { 
	-- | The name of the class the overloaded var is in.
          infoOverloadedClassName	:: Var

	-- | Class parameter variables. 
	, infoOverloadedClassParams	:: [(Var, Kind)]

	-- | The overloaded value variable.
	, infoOverloadedVar		:: Var

	-- | Type of the overloaded function, from the class declaration.
	, infoOverloadedType		:: Type 

	-- | All instances of the class.
	, infoInstances			:: Seq Top }


instance Pretty InfoOverloaded PMode where
 ppr io	= vcat
	[ "className   = " % infoOverloadedClassName io
	, "classParams = " % infoOverloadedClassParams io
	, "var         = " % infoOverloadedVar io
	, "type        = " % infoOverloadedType io
	, ppr "instances:"
	, ppr $ infoInstances io 
	]


-- Lookup Overloaded ------------------------------------------------------------------------------
-- | Lookup information about an overloaded variable.
--	If the variable isn't overloaded then return Nothing.
lookupOverloadedVar :: Env -> Var -> Maybe InfoOverloaded
lookupOverloadedVar env vOverloaded

	-- See if this is an overloaded function by looking it up in the method table.
	--	If it is overloaded, then we'll get back the name of the type
	--	class that it was defined in.
	| Just vClass	<- takeFirstJust
				$ map (Map.lookup vOverloaded . globClassMethods)
				$ envGlobs env

		-- Get the class declaration where the overloaded var was defined.
	= let	pClassDict@PClassDict{}
			= fromMaybe (panic stage $ "Can't find class decl for class " % vClass)
			$ takeFirstJust
			$ map (Map.lookup vClass . globClassDict)
			$ envGlobs env

		-- Get the type of the overloaded var.
		Just tOverloaded 
			= lookup vOverloaded
		 	$ topClassDictTypes pClassDict 

		-- The type scheme in the class declaration doesn't include the context
		--	of the enclosing class, or quantifiers for its parameter variables.
		--	We add that stuff here.
		--	eg: for "show" need to add the "forall a. Show a =>" part to its type.
		--	TODO: It'd be better to do this during desugaring instead.
		super	 = unflattenSuper (map snd $ topClassDictParams pClassDict) SProp
		kContext = makeKApps 
				(KCon (KiConVar vClass) super)
				[TVar k $ UVar v | (v, k) <- topClassDictParams pClassDict ]
			 		
		tOverloaded_withContext
			= makeTForall_front 
				(topClassDictParams pClassDict)
				(addContext kContext tOverloaded)

		-- Get the sequence of instance declarations for the type class.
		pssInstances
			= mapMaybe (Map.lookup vClass . globClassInst)
			$ envGlobs env

		-- Concat the sequences of instances from all globs.
		psInstances
			= foldr (Seq.><) Seq.empty pssInstances
		
	  in	Just InfoOverloaded
			{ infoOverloadedClassName	= vClass
			, infoOverloadedClassParams	= topClassDictParams pClassDict
			, infoOverloadedVar		= vOverloaded
			, infoOverloadedType		= tOverloaded_withContext
			, infoInstances			= psInstances }
				
	| otherwise
	= Nothing
