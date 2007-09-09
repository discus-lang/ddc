
module Type.Scheme
	( lookupScheme
	, findType
	, collectCidsEnv
	, extractType
	, generaliseType)
where

import	Type.Exp
import 	Type.State


lookupScheme 
	:: Var -> SquidM (Maybe Type)

findType 
	:: Var	-> SquidM Type

collectCidsEnv
	:: Var
	-> [Var] 
	-> SquidM [ClassId]

extractType 
	:: Var 
	-> [ClassId] 
	-> SquidM Type

generaliseType
	:: Var 
	-> Type
	-> [ClassId]
	-> SquidM Type

