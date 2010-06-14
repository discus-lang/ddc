module Core.Reconstruct.Apply
	( applyValueT
	, applyTypeT)
where
import Core.Util
import Core.Reconstruct.Environment
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import Util

stage	= "Core.Reconstruct.Apply"


-- | Determine the result type and latent effect resulting from a value application.
applyValueT 
	:: Env			-- ^ Type environment.
	-> Type 		-- ^ Type of function to apply.
	-> Type 		-- ^ Type of argument.
	-> Maybe 		
		( Type		-- result type
		, Effect)	-- effect caused by application.

applyValueT env t1 t2
 	= applyValueT' env
		(packT $ toFetterFormT $ flattenT_constrainForm $ toConstrainFormT t1) 
		(packT $ toFetterFormT $ flattenT_constrainForm $ toConstrainFormT t2)

applyValueT' env t1@(TFetters t1Shape fs) t2
 = let 	([[], fsMore], [])	= partitionFs [isFWhere, isFMore] fs
	env'			= foldr addMoreF env fsMore
   in	applyValueT' env' t1Shape t2
 
applyValueT' env t0 t3	
 | Just (t1, t2, eff, clo)	<- takeTFun t0
 = let	result
	 | t1 == t3
	 = Just (t2, eff)

	 -- We're currenly using the subsumption judgement more than we really need to.
	 | subsumes ((flip lookupMoreVT) env) t1 t3
	 = Just (t2, eff)

	 | otherwise
	 = freakout stage
	   (vcat [ ppr "applyValueT: Type error in value application."
		 , "    called by = " 		% getEnvCaller env
		 , "    can't apply argument:\n"	%> t3
		 , "    to:\n"        		%> t0
		 , blank
		 , "    as it is not <: than:\n"	%> t1
		 , blank
		 , ppr "    with bounds:"])
--		 , vcat $ ["    " % v % " :> " % b | (v, b) <- Map.toList $ envMore env]])
		 Nothing

  in	result


applyValueT' env t1 t2
	= freakout stage
	  (vcat	[ ppr "applyValueT: No match for (t1 t2)."
		, "    t1 = " % t1
		, "    t2 = " % t2 ])
	  Nothing
	
	
-- | Apply a value argument to a forall\/context type, yielding the result type.
--	TODO: check that the kinds\/contexts match as we apply.
--
applyTypeT :: Env -> Type -> Type -> Maybe Type
applyTypeT env t1@(TForall BNil k11 t12) t2
	-- witnesses must match
	| k2	<- kindOfType t2
	, packK k11 == packK k2
	= Just t12

	-- kinds don't match
	| k2	<- kindOfType t2
	= freakout stage
	  (vcat	[ ppr "applyTypeT: Kind error in type application."
		, "    caller = " 	% getEnvCaller env
		, "    can't apply\n"	%> t2
		, "    to\n"		%> t1
		, "    k11\n"		%> packK k11
		, "    K[t2]\n"		%> packK k2])
	 	Nothing

applyTypeT env (TForall (BVar v) k t1) t2
	| k == kindOfType t2
	= Just (substituteT (subSingleton v t2)	t1)

	| otherwise
	= freakout stage
	  (vcat	[ ppr "applyTypeT: Kind error in type application."
		, "    caller = " 	% getEnvCaller env
		, ppr "    in application:\n"
		, "(\\/ " % parens (v % " :: " % k) % " -> ...)" <> parens t2 %"\n"
		, blank
 		, "        type: "	% t2
		, "    has kind: "	% kindOfType t2
		, "    expected: "	% k])
		Nothing

applyTypeT env (TForall (BMore v tB) k t1) t2
	-- if the constraint is a closure then trim it first
	| k == kClosure
	, subsumes (flip lookupMoreVT env)
			(packT $ flattenT_constrainForm $ trimClosureC_constrainForm t2) 
			(packT $ flattenT_constrainForm $ trimClosureC_constrainForm tB)
	= Just (substituteT (subSingleton v t2) t1)

	-- check that the constraint is satisfied
	| subsumes (flip lookupMoreVT env) t2 tB
	= Just (substituteT (subSingleton v t2) t1)
	
	| otherwise
	= freakout stage
	  (vcat	[ ppr "applyTypeT: Kind error in type application.\n"
		, "    caller = "	% getEnvCaller env
		, "    in application: (\\/ " % v % " :> (" % tB % ") " % k % " -> ...)" % " (" % t2 % ")"
		, blank
		, "        type: " 	% t2
		, blank
	 	, "    is not :> "	% tB])
		Nothing
	
applyTypeT env (TFetters t1 fs) t
	| Just t1'	<- applyTypeT env t1 t
	= Just $ TFetters t1' fs
	
applyTypeT env t1 t2
	= freakout stage
	  (vcat	[ ppr "applyTypeT: Kind error in type application.\n"
		, "    caller = " 	% getEnvCaller env
		, "    can't apply\n"	%> t2
		, "    to\n"		%> t1])
		Nothing


subSingleton v t v'
	| TVar _ (UVar v3)	<- t
	, v == v3	= Nothing

	| TVar _ (UMore v3 _)	<- t
	, v == v3	= Nothing
	
	| v == v'	= Just t
	| otherwise	= Nothing
	
