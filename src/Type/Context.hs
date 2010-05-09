
-- | Functions for reducing the context of a type.

module Type.Context
	( reduceContextT 
	, matchInstance )
where
import Type.Exp
import Type.Builtin
import Type.Util
import Shared.VarPrim
import Util
import DDC.Var
import Type.Effect.MaskLocal	(visibleRsT)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- | Reduce the context of this type using the provided map of instance definitions.
reduceContextT 
	:: Map Var [Fetter]	-- (class var -> instances) for this class
	-> Type			-- the type to reduce  
	-> Type
	
reduceContextT classInst tt
 = case tt of
 	TFetters tShape fs
	 -> let fs'	= catMap (reduceContextF (flattenT tt) classInst) fs
	    in  case fs' of
	    		[]	-> tShape
			_	-> TFetters tShape fs'
			
	_ 		-> tt
	

reduceContextF 
	:: Type			-- the shape of the type
	-> Map Var [Fetter]	-- (class var -> instances) for each class
	-> Fetter		-- the constraint being used
	-> [Fetter]		-- maybe some new constraints

reduceContextF tShape classInstances ff

	-- We can remove Const, Lazy, Mutable, Direct context if they are on regions which are not present
	--	in the shape of the type.
	--
	--	These regions are local to the function and a local region binding will be introduced
	--	which will create the required witnesses.
	--	
	| FConstraint v [tR@(TVar kR r)]	<- ff
	, kR	== kRegion
	, elem v [primConst, primMutable, primMutable, primDirect]
	, not $ Set.member tR $ visibleRsT tShape
	= []

	-- We can remove type class constraints when we have a matching instance in the table
	--	These can be converted to a direct function call in the CoreIR, so we don't need to
	--	pass dictionaries at runtime.
	--
 	| FConstraint v ts	<- ff
	, Just instances	<- Map.lookup v classInstances
	, Just inst'		<- find (matchInstance ff) instances
	= []

	-- Purity constraints on bottom effects can be removed.
	--	This doesn't give us any useful information.
	| FConstraint v [TSum kE []]	<- ff
	, kE	== kEffect
	, v 	== primPure 
	= []

	-- Purity constraints on manifest effects can be discharged. 
	--	These can be reconstructed in the CoreIR by using the Const witnesses that will have
	--	been generated when the Purity constraint was crushed.
	| FConstraint v [t]		<- ff
	, v == primPure
	= case t of
		TSum kE _		
			| kE == kEffect		-> []
		TApp (TCon TyConEffect{}) _	-> []
		_				-> [ff]
	
	-- These compound fetters can be converted to their crushed forms.
	--	Although Type.Crush.Fetter also crushes fetters in the graph, if a scheme is generalised
	--	which contains a fetter acting on a monomorphic class, and then that class is updated,
	--	we'll get a non-crushed fetter when that scheme is re-extracted from the graph
	| FConstraint v [t]		<- ff
	= let	mFs	= crushFetterSingleNonPurify_directly v t
	  in	case mFs of
		 Nothing	-> [ff]
		 Just fs	-> fs


	-- have to keep this context
	| otherwise
	= [ff]


-- | Crush a non-purify directly.
--	"directly" means that we've got the whole type at hand, and
--	don't have to go tracing through the graph for it.
crushFetterSingleNonPurify_directly
	:: Var
	-> Type
	-> Maybe [Fetter]
	
crushFetterSingleNonPurify_directly vFetter tt

	-- lazy head
	| vFetter		== primLazyH
	, TApp TCon{} tR	<- tt
	= Just [FConstraint primLazy [tR]]
	
	| vFetter		== primLazyH
	, TApp t1 t2		<- tt
	= crushFetterSingleNonPurify_directly vFetter t1
	
	-- lazy head where the ctor has no region (ie LazyH Unit)
	| vFetter		== primLazyH
	, Just (v, k, [])	<- takeTData tt
	= Just []
	
	-- deep mutability
	| vFetter	== primMutableT
	, Just _	<- takeTData tt
	= let	(rs, ds)	= slurpVarsRD tt
		fsRegion	= map (\r -> FConstraint primMutable  [r]) rs
		fsData		= map (\d -> FConstraint primMutableT [d]) ds
	  in	Just $ fsRegion ++ fsData
	  
	-- deep const
	| vFetter	== primConstT
	, Just _	<- takeTData tt
	= let	(rs, ds)	= slurpVarsRD tt
		fsRegion	= map (\r -> FConstraint primConst  [r]) rs
		fsData		= map (\d -> FConstraint primConstT [d]) ds
	  in	Just $ fsRegion ++ fsData
	  
	-- couldn't crush it
	| otherwise
	= Nothing


-- Checks if an class instance supports a certain type.
--	The class instance has to be more polymorphic than the type we want to support.
--
--	This is duplicated in Core.Dictionary for Core.Type
--
matchInstance 
	:: Fetter 	-- the class we want to support
	-> Fetter	-- the class of the instance
	-> Bool

matchInstance cType cInst
	| FConstraint v1 ts1		<- cType
	, FConstraint v2 ts2		<- cInst

	-- check the class is the same
	, v1 == v2
	, length ts1 == length ts2

	-- all the type arguments of the class must unify
	, Just constrs		<- sequence $ zipWith unifyTypes ts1 ts2

	-- any extra constraint from the unification must have 
	--	a var or wildcard for the RHS
	, and 	$ map (\(ta, tb) -> case tb of
				TVar  k v
				 | kindOfType ta == Just k
			 	 -> True

				_	-> False)

		$ concat $ constrs
	= True

	| otherwise
	= False






