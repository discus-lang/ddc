{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Construction and elaboration of data type definitons.
module DDC.Type.Data.CtorType
	(makeCtorType)
where
import DDC.Type.Kind
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Operators.Elaborate
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Util.FreeVars
import Data.Maybe
import qualified Data.Set	as Set
import qualified Shared.VarPrim	as Var
import qualified Shared.VarUtil	as Var

stage	= "DDC.Type.Data.CtorType"

-- | Make the type of a data constructor, from a list of its paramter types.
--   This also fills in the closure information and any required constraints
--   on effect and closure variables.
makeCtorType 
	:: Monad m
	=> (NameSpace -> m Var)		-- ^ Function to allocate fresh variables.
	-> Var				-- ^ Name of data type constructor.
	-> [Var]			-- ^ Parameters to data type constructor.
	-> Var				-- ^ Name of data constructor.
	-> [Type]			-- ^ Parameters to data constructor.
	-> m Type

makeCtorType newVarN vData vsParam _ tsParam
 = do
	-- Ensure there we have variables in all the effect and closure positions of 
	-- fields with functional types.
--	tsParam_varified <- mapM (transformTM (replaceBotByVar newVarN)) tsParam

	-- Gather up all the vars from the field type.
	let vsFree	= Set.filter (not . Var.isCtorName)
			$ freeVars tsParam

	-- Check for vars in the field type aren't params of the data type.
	--	If they are effects or closures we can force them to be Bot with Pure / Empty fetters.
	let fsField	= catMaybes 
			$ map (checkAndConstrainVar vsParam) 
			$ Set.toList vsFree
			
	-- The kind of the type constructor
	let kData	= makeDataKind vsParam

	-- Make the type of the constructed object.
 	let objType	= makeTData vData kData
			$ map (\v -> case varNameSpace v of
					NameEffect	-> TVar kEffect  $ UVar v
					NameRegion	-> TVar kRegion  $ UVar v
					NameClosure	-> TVar kClosure $ UVar v
					NameType	-> TVar kValue   $ UVar v
					_		-> panic stage $ "makeCtorType: no match")
			$ vsParam

	-- As constructors don't inspect their arguments, they are all pure, 
	-- so we don't need to add any effects. We still need to add closure
	-- information though incase they are partially applied.
 	tCtor		<- elaborateCloT newVarN
			$  makeTFunsPureEmpty (tsParam ++ [objType])

	-- Add forall quantifiers to the front of the body type to make the final scheme.
	let bks		= map (\v -> (BVar v, let Just k = defaultKindOfVar v in k)) 
			$ vsParam

	let tCtor_quant	= makeTForall_back bks (addConstraintsOther fsField tCtor)

	return 	tCtor_quant

{-
-- | If this type is a bottom then replace it by a fresh variable.
replaceBotByVar
	:: Monad m 
	=> (NameSpace -> m Var)	-- ^ Function to call to allocate fresh names.
	-> Type	-> m Type
	
replaceBotByVar  newVarN tt
 = case tt of
 	TSum k []
	 -> do	let Just nameSpace = spaceOfKind k
		v	<- newVarN nameSpace
	 	return	$ TVar k $ UVar v

	_ ->	return tt
-}

-- | Check that a type varible is present in the list of parameters to a data type.
--	If it's not, then for effect and closure variables we can just constrain
--	the type of the constructor with Pure or Empty to ensure it stays Bot.
--	For vars of other kinds, die with a user error.
--
checkAndConstrainVar
	:: [Var]		-- ^ Parameters of the data type.
	-> Var			-- ^ Variable to check.
	-> Maybe Fetter		-- ^ Extra constraint to add to the data constructor.
	
checkAndConstrainVar vs v
	| elem v vs
	= Nothing
	
	-- effect vars not present in the data type can be made pure
	| varNameSpace v == NameEffect
	= Just $ FConstraint Var.primPure  [TVar kEffect $ UVar v]
	
	-- closure vars not present in the data type can be made empty
	| varNameSpace v == NameClosure
	= Just $ FConstraint Var.primEmpty [TVar kClosure $ UVar v]
	
	| otherwise
	= dieWithUserError
		[ Var.prettyPos v % "\n"
		% "    Variable " % v % " is not present in the data type\n" ]
