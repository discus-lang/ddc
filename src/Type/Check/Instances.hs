
module Type.Check.Instances 
	(checkInstances)
where
import Type.Extract
import Shared.VarPrim
import Util
import DDC.Var
import DDC.Type
import DDC.Type.Operators.Context
import DDC.Solve.State
import DDC.Solve.Error
import qualified Data.Map	as Map
import qualified Data.Sequence	as Seq

debug		= True
trace ss 	= if debug then traceM ss else return ()

-- | Check that all FConstraints in the graph have a corresponding instance.
--	If we don't catch undefined instances here then Core.Dictionary will
--	panic when trying to resolve the overloaded calls.
checkInstances :: SquidM ()
checkInstances 
 = do	errs	<- foldClasses checkInstances1 []
	addErrors errs

checkInstances1 errs cls
 	| Class {}		<- cls
	= foldM (checkFetter cls) errs 
		$ [FConstraint vFetter [TVar (classKind cls) $ UClass (classId cls)]
			| vFetter <- Map.keys $ classFetters cls]
	
	| ClassFetter { classFetter = f }	<- cls
	, FConstraint v t	<- f
	= checkFetter cls errs f
	
	| otherwise
	= return errs
	
checkFetter cls errs f@(FConstraint vClass tsArg)
 = do	trace	$ "*   checkFetter: checking " % f % "\n"
	let cidsArg	= map (\(TVar k (UClass cid)) -> cid) tsArg
	
	-- extract the types for the constraint params
	Just tsArg_ex	<- liftM sequence
			$  mapM (extractTypeCid True) cidsArg
	
	-- instances are only defined on the shape of the type, 
	--	not additional fetters.
	let tsArg'	= map takeShapeT tsArg_ex
	
	let f'	= FConstraint vClass tsArg'
	trace	$ "    f' = " % f'	% "\n"

	-- see if we have an instance for this constraint.
	classInst	<- getsRef stateClassInst

	let (result :: SquidM [Error])
		-- these aren't type class constraints, so aren't a problem.
		| elem vClass 
			[ primMutable, primMutableT, primConst, primConstT
			, primPure, primEmpty
			, primLazy, primLazyH, primDirect]
		= return errs
		
		| varName vClass == "Safe"
		= return errs

		-- constraint has an instances, ok.
		| Just instances	<- Map.lookup vClass classInst
		, Just _		<- find (matchInstance f') instances
		= return errs
		
		-- couldn't find the instance in the table,
		--	add an error to the solver state and carry on.
		| otherwise
		= return 
			$ ErrorNoInstance
	  		  { eClassVar 		= vClass
			  , eTypeArgs		= tsArg' 
			  , eFetterMaybeSrc	= Just $ takeFetterSrc cls f' }
			: errs
	result

takeShapeT tt
 = case tt of
 	TConstrain t _	-> t
	_		-> tt


takeFetterSrc cls f@(FConstraint vClass _)
 = case cls of
 	ClassFetter { classSource = src }	
	 -> src

	Class { classFetters = fetters }
	 -> let Just (src Seq.:< _)	
			= liftM Seq.viewl
			$ Map.lookup vClass fetters
	    in  src

