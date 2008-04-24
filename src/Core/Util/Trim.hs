
-- | Closure trimming for core types.
module Core.Util.Trim
	( trimClosureT
	, trimClosureC )

where	

import Core.Util.Bits
import Core.Util.Pack
import Core.Exp

import Type.Util.Bits
import Type.Util.Kind

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Pretty
import Shared.Error
import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug

debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x

-----
stage	= "Core.Util.Trim"

-- | Trim the closure portion of this type
trimClosureT :: Type -> Type
trimClosureT tt = {-# SCC "trimClosureT" #-} trimClosureT2 tt

trimClosureT2 tt
  = let	tt'	= packT $ trimClosureT' tt
    in	if tt' == tt
    		then tt'
		else trimClosureT2 tt'

trimClosureT' tt		
 = case tt of
 	TFetters t fs	
	 -> makeTFetters t (catMaybes $ map (trimClosureT_f Set.empty Set.empty) fs)

	_		-> tt
	
-- | Trim the closure in this binding
--	where the binding was on a type
trimClosureT_f :: Set Var -> Set Var -> Fetter -> Maybe Fetter
trimClosureT_f quant rsData ff
 = case ff of
 	FWhere v t2
	 |  kindOfType t2 == Just KClosure
	 -> Just $ FWhere v (trimClosureC quant rsData t2)

 	FMore v t2
	 |  kindOfType t2 == Just KClosure
	 -> Just $ FMore v (trimClosureC quant rsData t2)

	_ -> Just ff


-- | Trim a closure term down to its interesting parts.
--	Much of the information from the types of free variables isn't interesting to 
--	the type system.
--
--	We only actually care about which regions from the environment are being,
--	referenced by a particular object. We don't care about its actual value type,
--	or what effects a particular function might have.
--
--	eg in this closure term:
--		x : forall %r1 %r3. Int %r1 -(y : Int %r2)> Int %r3
--
--		Only %r2 and %r3 represent information which is shared with the
--		environment.
--
trimClosureC 
	:: Set Var 	-- which variables are considered quanfified by the closure
			--	and can be dicarded.
	
	-> Set Var	-- the tag regions of type constructors that this closure
			--	is a part of (for tracking dangerous vars)

	-> Closure 	-- the closure to trim
	-> Closure	-- trimmed closure

trimClosureC quant rsData cc 
 = {-# SCC "trimClosureC" #-} 
   let cc_trimmed	= trimClosureC2 quant rsData cc
   in  trace 	( "trimClosureC:\n"
   		% "   cc:\n" 		%> cc		% "\n"
		% "   cc_trimmed:\n"	%> cc_trimmed	% "\n")
		cc_trimmed
		

-- keep packing and trimming until it won't trim anymore
trimClosureC2 quant rsData cc
 | isClosure cc
  = let cc'	= trimClosureC' quant rsData cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC2 quant rsData cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' quant rsData cc
 = let down	= trimClosureC quant rsData
   in case cc of

	-- vars which are quantified in this closure aren't free and can be trimmed out
 	TVar KClosure v
	 | Set.member v quant	-> TBot KClosure
	 | otherwise		-> cc
	 
	TVarMore KClosure v cMore
	 | Set.member v quant	-> TBot KClosure
	 | otherwise		-> TVarMore KClosure v $ down cMore


	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map down
		$  flattenTSum cc

	TMask KClosure t1 t2
	 -> let t1'	= down t1
	 	result
	 		| TVar k v	<- t1'
			, Set.member v quant
			= t1'
			
			| otherwise
			= applyTMask (TMask KClosure t1' t2)
	    in	result						

	TFetters c fs
	 -> makeTFetters
		(down c)
		(catMaybes $ map (trimClosureC_f quant rsData) fs) 

	-- Erase the quantifier if the var is no longer free in the type
	TForall b k t		
	 -> let quant'	= Set.insert (varOfBind b) quant
	    in  trimClosureC quant' rsData t

	TFree tag (TVar k v)
		| k == KEffect
		-> TBot KClosure
		
		-- If this closure has no free variables
		--	then it is closed and can safely be erased.
		| Set.member v quant
		-> TBot KClosure
		
	TFree tag (TBot _)
		-> TBot KClosure
		
	-- Trim either a data or closure element of a closure
	--	We need this dispatch because the right hand side of a 
	--	TFree can be either data or more closure
	TFree tag t
	  | isClosure t
	  -> TFree tag $ down t

	  | otherwise
	  -> makeTSum KClosure 
		$ map (TFree tag) 
		$ trimClosureC_t quant rsData t
			
	TTag   v		-> cc


	_ -> panic stage
		$ "trimClosureC: no match for " % show cc


-- | Trim a data element of a closure.
trimClosureC_t :: Set Var -> Set Var -> Type -> [Type]
trimClosureC_t quant rsData tt
 = let down = trimClosureC_t quant rsData
   in  case tt of
	TVar k v	
		| Set.member v quant	-> []
		| otherwise		-> [tt]

	TVarMore k v t
		| Set.member v quant	-> []
		| otherwise		-> [tt]


	-- Trim the fetters of this data
	-- BUGS: we sometimes get fetters relating to :> constraints on effects, but we shouldn't
 	TFetters c fs
	 -> map (\t -> makeTFetters t (catMaybes $ map (trimClosureC_f quant rsData) fs))
	    		$ down c

	-- Trim under foralls
	TForall b k t		
	 -> let quant'	= Set.insert (varOfBind b) quant
	    in  trimClosureC_t quant' rsData t

	TSum k ts	-> catMap down ts
	TMask k t1 t2	-> [TMask k (makeTSum k $ down t1) t2]
	
	TCon{}		-> []

	TBot{}		-> []
	TTop{}		-> [tt]

	-- TODO: We don't know if which of these type are actually tangible data, so we have
	--       to assume they all are. If a type is only used as the parameter/return of a
	--       function then we could trim it out here, but it shouldn't hurt too much to 
	--       leave it in. (it's always safe to _increase_ the closure)
	--
	TApp{}
	 -> let result
			| Just (v, k, ts)	<- takeTData tt
			= catMap down ts
			
			-- An object of type (t1 -($c1)> t2) does not contain a value of 
			-- either type t1 or t2.  nly the closure portion of a function actually holds data.
			| Just (t1, t2, eff, clo) <- takeTFun tt
			= down clo
			
			| otherwise
			= panic stage
			$ "trimClosureC_t: no match for (" % tt % ")"
	   in result

	TEffect{}	-> []
	TFree v t	-> [trimClosureC quant rsData tt]
	 	 
	-- Don't care about contexts
	TContext t1 t2
	 -> down t2

	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter of a closure
trimClosureC_f :: Set Var -> Set Var -> Fetter -> Maybe Fetter
trimClosureC_f quant rsData ff
 = case ff of
	-- Only more closure information is interesting
 	FWhere t1 c2
	 |  kindOfType c2 == Just KClosure
	 -> Just $ FWhere t1 (trimClosureC quant rsData c2)

 	FMore t1 c2
	 |  kindOfType c2 == Just KClosure
	 -> Just $ FMore t1 (trimClosureC quant rsData c2)

	_ -> Nothing

	
