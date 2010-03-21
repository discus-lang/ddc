-- | Pack a type into standard form.
module Type.Util.PackFast
	( packType
	, packType_markLoops )
where
import Type.Exp
import Type.Util.Bits
import Type.Util.Kind
import Type.Pretty		()
import Type.Plate.Collect
import Shared.Error
import Shared.Pretty
import Util
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Debug.Trace	as Debug


-----
stage	= "Type.Util.PackFast"
debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x


-- | Controls how the type is packed.
data Config
	= Config {

	-- | Whether substitute for effect and closure equality constraints.
	-- 	In the core types we always do this.
	-- 	In source types we don't substitute into the body of the type, 
	--	but we do into other constraints. This is always turned on when the
	--	packer enters into constraints in a TConstrain.
	  configSubForEffClo	:: Bool

	-- | Whether to crush built-in effects and type class constraints 
	--	like ReadT and LazyH on the way through
	, configCrush		:: Bool
	
	-- | Whether to panic if we hit a loop through the value portion of the type graph.
	, configLoopResolution	:: ConfigLoopResolution
	}	
	deriving (Show)


-- | Says what to do if we hit a loop in through the value portion of the type graph.
--	Note: loops through the effect and closure portion aren't errors,
--	      and we'll always get this with recursive functions.
data ConfigLoopResolution
	= ConfigLoopPanic	-- ^ throw a compiler panic.
	| ConfigLoopTError	-- ^ leave an embedded TError in the type explaining the problem.
	| ConfigLoopNoSub	-- ^ don't substitute into the looping variable.
	deriving (Show)


-- | This gets called often by the constraint solver, so needs to be reasonably efficient.
--	The input type needs to be in "TConstrain" form
--
-- TODO: This is does a naive substitution.
--	 It'd be better to use destructive update to implement the substitution, 
--	 then eat up all the IORefs in a second pass.
--
packType :: Type -> Type
packType tt	
 = let	config	= Config
		{ configSubForEffClo	= False
		, configCrush		= False
		, configLoopResolution	= ConfigLoopPanic }

   in	packTypeCrsSub config Map.empty Set.empty tt


-- | Pack a type into normal form. 
--	If we find a loop through the value portion of the type graph then
--	leave a TError explaining the problem.
packType_markLoops :: Type -> Type
packType_markLoops tt
 = let	config	= Config
		{ configSubForEffClo	= False
		, configCrush		= False
		, configLoopResolution	= ConfigLoopTError }
	
   in	packTypeCrsSub config Map.empty Set.empty tt


-- | Pack constraints into a type.
packTypeCrsSub 
	:: Config
	-> Map Type Type		-- ^ all the eq constraints to substitute.
	-> Set Type			-- ^ vars of constraints already subsituted in this context.
	-> Type				-- ^ type to pack into.
	-> Type

packTypeCrsSub config crsEq subbed tt
 = let tt'	= packTypeCrsSub' config crsEq subbed tt
   in  trace 	( "packTypeCrsSub " % subbed % "\n" 
		% "    tt  = " % tt 	% "\n"
		% "    tt' = " % tt'	% "\n")
		tt'

packTypeCrsSub' config crsEq subbed tt
 = case tt of

	-- decend into foralls
	TForall v k t
	 -> TForall v k $ packTypeCrsSub config crsEq subbed t
	
	-- the old packed handes TFetters.
	--	we're factoring it out.
	TFetters t fs
	 -> panic stage 
  	  $  "packType: doesn't handle TFetters"
	  %  " tt = " % tt % "\n"
	
	TConstrain (TForall b k t) crs
	 -> TForall b k (addConstraints crs t)
	
	-- In a constrained type, all the equality constraints are inlined,
	--	but we keep all the "more than" and type class constraints.
	--
	TConstrain t crs@(Constraints crsEq2 crsMore2 crsOther2)
	 -> let	
		-- collect constraints on the way down.
		crsEq_all	= Map.union crsEq crsEq2
		
		-- pack equality constraints into the body of the type.
		t'		= packTypeCrsSub config crsEq_all subbed t

		-- pack equality constraints into the other sorts of constraints
		config_subEffClo = config { configSubForEffClo = True }

		crsMore2'	= Map.map (packTypeCrsSub  config_subEffClo crsEq_all subbed) crsMore2
		crsOther2'	=     map (packTypeCrsSubF config_subEffClo crsEq_all subbed) crsOther2

		-- Restrict equality constraints to only those that might be reachable from
		--	the body of the type. Remember that packing is done on types
		--	in both weak and non-weak forms, and with and without
		--	embedded ClassIds. Also drop boring constraints while we're here.
		freeClassVars	 = collectTClassVars t'
		
		crsEq2_restrict	 
			= Map.filterWithKey
			 	(\t1 t2 ->  Set.member t1 freeClassVars
				      && (not $ isBoringEqConstraint t1 t2))
				crsEq2 
			
		-- pack equality constraints into the others.							
		crsEq2_restrict' 
			= Map.map (packTypeCrsSub config_subEffClo crsEq_all subbed) crsEq2_restrict

		-- the final constraints
		crs'	= Constraints crsEq2_restrict' crsMore2' (nub crsOther2')

	    in	addConstraints crs' t'
	
	-- TODO: I'm pretty sure this makeTSum give us at least O(n^2) complexity.
	--	 It'd be better to accumulate a set of effects on the way down.
	TSum k ts
	 -> let ts'	= map (packTypeCrsSub config crsEq subbed) ts
	    in	makeTSum k ts'
	
	TApp t1 t2
	 -> let	t1'	= packTypeCrsSub config crsEq subbed t1
		t2'	= packTypeCrsSub config crsEq subbed t2
	    in	TApp t1' t2'
	
	TCon{}	-> tt
	TBot{}	-> tt
	TTop{}	-> tt
	
	TEffect v ts
	 -> let ts'	= map (packTypeCrsSub config crsEq subbed) ts
	    in	TEffect v ts'
	
	-- for a closure like  v1 : v2 : TYPE, 
	--	the type is really a part of v1. The fact that it also came from v2
	--	doesn't matter. The variables are just for doccumentaiton anyway.
	TFree v1 t2@(TFree _ t)
	 -> packTypeCrsSub config crsEq subbed (TFree v1 t)

	TFree v1 t2@(TConstrain t crs)
	 -> TConstrain (TFree v1 t) crs

--	TFree v1 t2@(TConstrain (TFree v2 t) crs)
--	 -> packTypeCrsSub config crsEq subbed (TFree v1 (TConstrain t crs))

	TFree v1 t2@(TSum k ts)
	 | k == kClosure
	 -> TSum k 
	  $ map (packTypeCrsSub config crsEq subbed)
	  $ map (TFree v1) ts
	
	TFree v t
	 -> let t'	= packTypeCrsSub config crsEq subbed t
	    in	TFree v t'
	
	TDanger t1 t2
	 -> let t2'	= packTypeCrsSub config crsEq subbed t2
	    in	TDanger t1 t2'
	
	TVar   k v	-> packTypeCrsClassVar config crsEq subbed tt k	
	TClass k cid	-> packTypeCrsClassVar config crsEq subbed tt k

	TError{}	-> tt

	_ -> panic stage
		$ "packType: no match for " % show tt

-- | Pack constraints into a type variable.
packTypeCrsClassVar
	:: Config
	-> Map Type Type	-- ^ all the eq constraints to substitute
	-> Set Type		-- ^ vars of constraints already subsituted in this context
	-> Type			-- ^ the type variable
	-> Kind			-- ^ kind of the type variable
	-> Type

packTypeCrsClassVar config crsEq subbed tt k
	-- if we're not substituting for effects or closures, then don't
	| k == kEffect || k == kClosure
	, not $ configSubForEffClo config
	= tt

	-- we've already substituted for this var on the same path
	| Set.member tt subbed
	= if k == kEffect || k == kClosure

		-- for effect and closure constraint's that's ok. 
		-- we'll always get loops with recursive functions.
		then tt

		-- we don't support recursive value types.
		else packTypeCrsClassVar_loop config crsEq subbed tt k
		
	 -- do the substitution
	 | otherwise
	 = case Map.lookup tt crsEq of
		Just t		-> packTypeCrsSub config crsEq (Set.insert tt subbed) t
		Nothing		-> tt


-- we've found a loop through the value portion of the type graph.
packTypeCrsClassVar_loop config crsEq subbed tt k
 = case configLoopResolution config of

	-- Uh oh, we weren't expecting any loops at the moment.
	ConfigLoopPanic
	 -> panic stage ("packType loop through " % tt) 

	-- Just leave the variable there and don't substitute.
	ConfigLoopNoSub
	 -> tt
	
	-- We've hit a loop, but we want to work out what constraint the loop is through.
	ConfigLoopTError
	 -> let	
		-- pack constraints into the looping variable, but just leave further
		--	recursive occurrences unsubstituted.
		config'	= config { configLoopResolution = ConfigLoopNoSub }
		tLoop	= packTypeCrsSub config' crsEq Set.empty tt
		Just k	= kindOfType tt

	    in	TError k (TypeErrorLoop tt tLoop)
		

-- | Pack constraints into a fetter.
packTypeCrsSubF
	:: Config
	-> Map Type Type	-- ^ all the eq constraints to substitute
	-> Set Type		-- ^ vars of constraints already subsituted in this context
	-> Fetter		-- ^ fetter to pack into
	-> Fetter

packTypeCrsSubF config crsEq subbed ff
 = case ff of
	FConstraint v ts
	 -> let	ts'	= map (packTypeCrsSub config crsEq subbed) ts
	    in	FConstraint v ts'
	
	FProj tProj vInst t1 t2
	 -> let	t1'	= packTypeCrsSub config crsEq subbed t1
		t2'	= packTypeCrsSub config crsEq subbed t2
	    in	FProj tProj vInst t1' t2'
	
	_ -> panic stage
		$ "packTypeF: no match for " % show ff


-- | Constraining an effect or closure to bot doesn't tell us anything we didn't
--	already know, so we can just drop it.
isBoringEqConstraint :: Type -> Type -> Bool
isBoringEqConstraint t1 t2
 = case t2 of
	TBot k
	 ->  k == kEffect
	 ||  k == kClosure
	
	-- types in TFrees can be rewritten to TBot by the closure trimmer.
	TFree _ (TBot k)
	 -> k == kClosure
	
	-- constraint is interesting.
	_	-> False

