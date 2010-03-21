
module Type.Check.Instances 
	(checkInstances)
where
import Type.Context
import Type.Error
import Type.Extract
import Type.Class
import Type.State
import Type.Exp
import Shared.VarPrim
import Util
import qualified Shared.Var	as Var
import qualified Data.Map	as Map


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
 	| Class { classFetterSources = fs_src }		<- cls
	= foldM (checkFetter cls) errs $ map fst fs_src
	
	| ClassFetter { classFetter = f }	<- cls
	, FConstraint v t	<- f
	= checkFetter cls errs f
	
	| otherwise
	= return errs
	
checkFetter cls errs f@(FConstraint vClass tsArg)
 = do	trace	$ "*   checkFetter: checking " % f % "\n"
	let cidsArg	= map (\(TClass k cid) -> cid) tsArg
	
	-- extract the types for the constraint params
	Just tsArg_ex	<- liftM sequence
			$  mapM (extractTypeCid True) cidsArg
	
	-- instances are only defined on the shape of the type, 
	--	not additional fetters.
	let tsArg'	= map takeShapeT tsArg_ex
	
	let f'	= FConstraint vClass tsArg'
	trace	$ "    f' = " % f'	% "\n"

	-- see if we have an instance for this constraint.
	classInst	<- gets stateClassInst

	let (result :: SquidM [Error])
		-- these aren't type class constraints, so aren't a problem.
		| elem vClass 
			[ primMutable, primMutableT, primConst, primConstT
			, primPure, primEmpty
			, primLazy, primLazyH, primDirect]
		= return errs
		
		| Var.name vClass == "Safe"
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
 	TFetters t fs	-> t
	_		-> tt


takeFetterSrc cls f@(FConstraint vClass _)
 = case cls of
 	ClassFetter { classSource = src }	
	 -> src

	Class { classFetterSources = nodes }
	 -> let Just (_, src) = find (isFNode vClass) nodes
	    in  src


isFNode v ff
 = case ff of
 	(FConstraint v' _, _ )	-> True
	_			-> False
