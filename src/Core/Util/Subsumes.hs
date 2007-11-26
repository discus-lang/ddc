
module Core.Util.Subsumes
	(subsumes)

where

import Core.Exp

-- | Check if one type subsumes another
--	BUGS: assumes that effect and closures in data types are all covariant
--
subsumes :: Type -> Type -> Bool
subsumes t s

	-- sums
	| TSum tKind ts		<- t
	, TSum sKind ss		<- s
	, tKind == sKind
	, and $ map (\s -> elem s ts) ss
	= True
	
	-- masks
	| TMask k t1 t2		<- t
	, TMask k s1 s2		<- s
	, subsumes t1 s1
	, t2 == s2
	= True 

	-- var
	| TVar tKind tVar	<- t
	, TVar sKind sVar	<- s
	, tKind == sKind
	, tVar == sVar
	= True

	-- bottoms match
	| TBot tKind		<- t
	, TBot sKind		<- s
	= tKind == sKind
	
	-- top subsumes everything
	| TTop tKind		<- t
	= True

	-- fun
 	| TFunEC t1 t2 tEff tClo	<- t
	, TFunEC s1 s2 sEff sClo	<- s
	, subsumes s1 t1
	, subsumes t2 s2
	, subsumes tEff sEff
	, subsumes tClo sClo
	= True
	
	-- data 
	| TData tVar ts		<- t
	, TData sVar ss		<- s
	, tVar == sVar
	, length ts == length ss
	, and $ zipWith subsumes ts ss
	= True
	
	-- effect constructor
	| TEffect tVar ts	<- t
	, TEffect sVar ss	<- s
	, tVar == sVar
	, ts == ss
	= True
	
	-- closure constructor
	| TFree tVar ts		<- t
	, TFree sVar ss		<- s
	, tVar == sVar
	, ts == ss
	= True
	
	-- classs
	| TClass tVar ts	<- t
	, TClass sVar ss	<- s
	, tVar == sVar
	, ts == ss
	= True
	
	-- effect sum / single constructor
	| TSum KEffect ts	<- t
	, TEffect tVar ss	<- s
	, elem s ts
	= True
	
	-- closure / single constructor
	| TSum KClosure ts	<- t
	, TFree v s		<- s
	, elem s ts
	= True
	
	-----
	| otherwise
	= False
	
{-	= panic stage
	$ "subsumes " % t % " " % s % "\n\n"
-}
