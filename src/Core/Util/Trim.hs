
-- | Closure trimming for core types.
module Core.Util.Trim
	( trimClosureT
	, trimClosureC )
where	
import Type.Util.Kind
import Util
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import qualified Data.Set	as Set
import qualified Debug.Trace	as Debug

stage		= "Core.Util.Trim"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain ss) x else x


-- | Trim the closure portion of this type
trimClosureT :: Type -> Type
trimClosureT tt = {-# SCC "trimClosureT" #-} trimClosureT2 tt

trimClosureT2 tt
  = let	tt'	= trimClosureT' tt
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
	 |  isClosure t2
	 -> Just $ FWhere v (trimClosureC quant rsData t2)

 	FMore v t2
	 |  isClosure t2
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
	% "    cc = " % cc 	% "\n"
	% show cc % "\n"
	


trimClosureC' quant rsData cc
 = let down	= trimClosureC quant rsData
   in case cc of

	-- vars which are quantified in this closure aren't free and can be trimmed out
 	TVar _ (UVar v)
	 | Set.member v quant	-> tEmpty
	 | otherwise		-> cc
	 
	TVar _ (UMore v cMore)
	 | Set.member v quant	-> tEmpty
	 | otherwise		-> TVar kClosure $ UMore v $ down cMore


	-- Trim all the elements of a sum
	TSum  _ cs	
	 -> makeTSum kClosure 
	 $  map down
	 $  flattenTSum cc

	TFetters c fs
	 -> makeTFetters
		(down c)
		(catMaybes $ map (trimClosureC_f quant rsData) fs) 

	
	-- Erase the quantifier if the var is no longer free in the type
	TForall BNil k t
	 -> trimClosureC quant rsData t

	TForall b k t		
	 -> let Just v	= takeVarOfBind b
		quant'	= Set.insert v quant
	    in  trimClosureC quant' rsData t

	TApp t1 t2

	 -- If this closure has no free variables
	 --	then it is closed and can safely be erased.
	 | Just (tag, TVar k (UVar v))		<- takeTFree cc
	 , k == kEffect	|| Set.member v quant	
	 -> tEmpty

	 | Just (tag, TSum _ [])	<- takeTFree cc
	 -> tEmpty
	
	 -- Trim either a data or closure element of a closure
	 --	We need this dispatch because the right hand side of a 
	 --	TFree can be either data or more closure
	 | Just (tag, t)		<- takeTFree cc
	 -> if isClosure t
		then makeTFree tag $ down t
		else makeTSum kClosure 
			$ map (makeTFree tag) 
			$ trimClosureC_t quant rsData t
				
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc


-- | Trim a data element of a closure.
trimClosureC_t :: Set Var -> Set Var -> Type -> [Type]
trimClosureC_t quant rsData tt
 = let down = trimClosureC_t quant rsData
   in  case tt of
	TVar k (UMore v _)
		| Set.member v quant	-> []
		| otherwise		-> [tt]

	TVar k (UVar v)
		| Set.member v quant	-> []
		| otherwise		-> [tt]

	-- Trim the fetters of this data
	-- BUGS: we sometimes get fetters relating to :> constraints on effects, but we shouldn't
 	TFetters c fs
	 -> map (\t -> makeTFetters t (catMaybes $ map (trimClosureC_f quant rsData) fs))
	    		$ down c

	-- Trim under foralls
	TForall BNil k t
	 -> down t
	
	TForall b k t		
	 -> let Just v	= takeVarOfBind b
		quant'	= Set.insert v quant
	    in  trimClosureC_t quant' rsData t

	TSum k ts	-> catMap down ts
	
	TCon{}		-> []

	-- TODO: We don't know if which of these type are actually tangible data, so we have
	--       to assume they all are. If a type is only used as the parameter/return of a
	--       function then we could trim it out here, but it shouldn't hurt too much to 
	--       leave it in. (it's always safe to _increase_ the closure)
	--
	TApp{}
	 | isEffect tt
	 -> []

	 | Just (v, k, ts)	<- takeTData tt
	 -> catMap down ts
			
	 -- An object of type (t1 -($c1)> t2) does not contain a value of 
	 -- either type t1 or t2.  nly the closure portion of a function actually holds data.
	 | Just (t1, t2, eff, clo) <- takeTFun tt
	 -> down clo
	
	 | Just (v, t)		<- takeTFree tt
	 -> [trimClosureC quant rsData tt]
	
	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter of a closure
trimClosureC_f :: Set Var -> Set Var -> Fetter -> Maybe Fetter
trimClosureC_f quant rsData ff
 = case ff of
	-- Only more closure information is interesting
 	FWhere t1 c2
	 |  isClosure t1
	 -> Just $ FWhere t1 (trimClosureC quant rsData c2)

 	FMore t1 c2
	 |  isClosure t1
	 -> Just $ FMore t1 (trimClosureC quant rsData c2)

	_ -> Nothing

	
