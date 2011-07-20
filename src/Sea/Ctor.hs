
-- | Use data type declarations to make code for each of the data constructors.
module Sea.Ctor
	(expandCtorTree)
where
import Util
import DDC.Base.DataFormat
import DDC.Main.Error
import DDC.Sea.Exp
import DDC.Var
import DDC.Var.PrimId
import Shared.VarUtil		(VarGenM, newVarN)
import qualified Shared.Unique	as Unique
import qualified Config.Config	as Config
import qualified Data.Map	as Map

type	ExM	= VarGenM

stage = "Sea.Ctor"

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

expandCtor ctor@(CtorDef vCtor tCtor arity tag fields types)
 = do	-- var of the constructed object.
	nObj		<- liftM NAuto $ newVarN NameValue

	-- Determine how the fields are going to be store. Constructors with
	-- one or more  unboxed fields use a different allocation method and
	-- layout to those only boxed fields.
	let boxedCount	= length $ filter typeIsBoxed types

	let useDataM	= hasUnboxedFields ctor

	let allocation	= if useDataM
			then PAllocDataM vCtor boxedCount (payloadSize types)
			else PAllocData vCtor arity

	-- allocate the object
	let allocS 	= SAssign (XVar nObj tPtrObj) tPtrObj (XPrim (MAlloc allocation) [])

	let expandField = if useDataM
			then expandFieldDataM vCtor
			else expandFieldData

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



-- | Calculate the size of the DataM payload
payloadSize types
 = let (boxed, unboxed) = partition typeIsBoxed types

   in	(length boxed) * Config.pointerBytes
	+ foldl' calculatePaddedLength 0 (map unboxedSize unboxed)


calculatePaddedLength :: Int -> Int -> Int
calculatePaddedLength accum fieldLen
 = if accum `mod` fieldLen == 0
	then accum + fieldLen
	else accum - (accum `div` fieldLen) + 2 * fieldLen


unboxedSize :: Type -> Int
unboxedSize (TCon (TyConUnboxed v))
 = case varId v of
	VarIdPrim (TChar (UnboxedBits 32))	-> 4

	VarIdPrim (TFloat (UnboxedBits 32))	-> 4
	VarIdPrim (TFloat (UnboxedBits 64))	-> 8

	VarIdPrim (TInt (UnboxedBits 8))	-> 1
	VarIdPrim (TInt (UnboxedBits 16))	-> 2
	VarIdPrim (TInt (UnboxedBits 32))	-> 4
	VarIdPrim (TInt (UnboxedBits 64))	-> 8

	VarIdPrim (TWord (UnboxedBits 8))	-> 1
	VarIdPrim (TWord (UnboxedBits 16))	-> 2
	VarIdPrim (TWord (UnboxedBits 32))	-> 4
	VarIdPrim (TWord (UnboxedBits 64))	-> 8

	id -> panic stage $ "unboxedSize : " ++ show id

