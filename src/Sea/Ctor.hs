
-- | Use data type declarations to make code for each of the data constructors.
module Sea.Ctor
	(expandCtorTree)
where
import Util
import DDC.Sea.Exp
import DDC.Var
import Shared.VarUtil		(VarGenM, newVarN)
import qualified Shared.Unique	as Unique
import qualified Config.Config	as Config
import qualified Data.Map	as Map

type	ExM	= VarGenM


-- | Expand the definitions of data constructors in this tree.
expandCtorTree :: Tree () -> Tree ()
expandCtorTree tree
	= evalState (liftM concat $ mapM expandDataP tree)
	$ VarId Unique.seaCtor 0


-- | Expand data constructors in a top level thing.
expandDataP :: Top ()	-> ExM [Top ()]
expandDataP p
 = case p of
 	PData v ctors
	 -> liftM concat
	  $ mapM (\(v, ctor) -> expandCtor ctor)
	  $ Map.toList ctors

	_		-> return [p]


-- | Expand the definition of a constructor.
expandCtor
	:: CtorDef
	-> ExM [Top ()]

expandCtor (CtorDef vCtor tCtor arity tag fields types)
 = do	-- var of the constructed object.
	nObj		<- liftM NAuto $ newVarN NameValue

	-- Determine how the fields are going to be store. Constructors with
	-- one or more  unboxed fields use a different allocation method and
	-- layout to those only boxed fields.
	let (boxed, unboxed)
			= partition typeIsBoxed types

	let allocation	= if null unboxed
			then PAllocData vCtor arity
			else PAllocDataM vCtor (length boxed)
					(dataMSize (length boxed) unboxed)

	-- allocate the object
	let allocS 	= SAssign (XVar nObj tPtrObj) tPtrObj (XPrim (MAlloc allocation) [])

	let expandField = if null unboxed
			then expandFieldData
			else expandFieldDataM vCtor


	-- Initialise all the fields.
	(stmtss, mArgVs)
		<- liftM unzip $ mapM (\ i -> expandField nObj i (types !! i)) [0 .. arity - 1]

	let fieldSs	= concat stmtss
	let argVs	= catMaybes mArgVs

	-- Return the result.
	let retS	= SReturn $ (XVar nObj tPtrObj)

	let stmts	= [allocS] ++ fieldSs ++ [retS]
	let super	= [PSuper vCtor argVs tPtrObj stmts]

	return 		$ super


-- | Create initialization code for this field
expandFieldDataM
	:: Var
	-> Name				-- ^ name of the object being constructed.
	-> Int				-- ^ index of argument.
	-> Type				-- ^ type of argument.
	-> ExM 	( [Stmt ()]		-- initialization code
		, Maybe (Var, Type))	-- the arguments to the constructor
					--	(will be Nothing if the field is secondary)
expandFieldDataM v nObj ixArg tArg
 = do	vArg	<- newVarN NameValue
	return	( [SAssign 	(XArgDataM v (XVar nObj tArg) ixArg)
				tPtrObj
				(XVar (NAuto vArg) tArg)]
		, Just (vArg, tArg) )


expandFieldData
	:: Name				-- ^ name of the object being constructed.
	-> Int				-- ^ index of argument.
	-> Type				-- ^ type of argument.
	-> ExM 	( [Stmt ()]		-- initialization code
		, Maybe (Var, Type))	-- the arguments to the constructor
					--	(will be Nothing if the field is secondary)
expandFieldData nObj ixArg tArg
 = do	vArg	<- newVarN NameValue
	return	( [SAssign 	(XArgData (XVar nObj tPtrObj) ixArg)
				tPtrObj
				(XVar (NAuto vArg) tArg)]
		, Just (vArg, tPtrObj) )




dataMSize boxCount unboxed
 = boxCount * Config.pointerBytes + (length unboxed) * 4
