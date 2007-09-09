
module Sea.Ctor
(
	expandCtorTree
)

where

-----
import Util

import qualified Shared.Unique	as Unique
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var

import Shared.Var		(Var, NameSpace(..))
import Shared.VarUtil		(VarGenM, newVarN)
import Shared.Error

import Sea.Exp

-----
stage	= "Sea.Ctor"

-----
type	ExM	= VarGenM 

-----
expandCtorTree ::	Tree () -> Tree ()
expandCtorTree	ts
	= evalState (liftM concat $ mapM expandDataP ts) 
	$ Var.XBind Unique.seaCtor 0
	

-----------------------
-- expandDataP
-- 	Expand data constructors.
--
expandDataP ::	Top ()	-> ExM [Top ()]
expandDataP	p
 = case p of
 	PData v ctors
	 -> liftM concat 
	  $ mapM (\(v, fs) -> expandCtor v fs) 
	  $ ctors

	_		-> return [p]
	
	
expandCtor v fields
 = do
	-- var of the constructed object.
	objV		<- newVarN NameValue

	-- allocation
	let allocS 	= SAssign (XVar objV) TObj 
			$ XAllocData v (length fields)

	-- field init
	(stmtss, mArgVs)
		<- liftM unzip
		$  zipWithM (expandField objV) [0..] fields

	let fieldSs	= concat stmtss
	let argVs	= catMaybes mArgVs

	-- return result
	let retS	= SReturn $ (XVar objV) 

	let stmts	= [allocS] ++ fieldSs ++ [retS]
	let super	= [PSuper v argVs TObj stmts]
	
	-- If this ctor has no fields, emit an atom def as well
	let atom	= if isNil fields
				then	[ PAtomProto v TObj
					, PAtom      v TObj]
				else 	[]
	
	return 		$ atom ++ super
 	

-----
expandField
	:: Var				-- ^ var of object being constructed.
	-> Int				-- ^ index of field.
	-> DataField [Stmt ()] Type	-- ^ field to build.
	-> ExM ([Stmt ()], Maybe (Var, Type))	
	
expandField objV ix field
	| dPrimary field
	= do	argV	<- newVarN NameValue
		return	( [SAssign (XArg (XVar objV) TData ix) (dType field) (XVar argV)]
			, Just (argV, TObj) )

	| not $ dPrimary field
	, Just ss		<- dInit field
	, Just (SStmt lastS)	<- takeLast ss
	= 	return	( init ss ++
			  [SAssign (XArg (XVar objV) TData ix) (dType field) lastS ]
	 		, Nothing )

	| not $ dPrimary field
	, Nothing	<- dInit field
	, Just name	<- dLabel field
	= panic stage $ "expandField: no initialization expression for non-primary field '" % name % "'"




