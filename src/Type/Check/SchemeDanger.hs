
-- All regions are assumed constant until proven mutable.
--	This means that we can generalise type variables in scheme but find
--	out they were actually are dangerous and non-generalisable later on.
--
-- If we just instantiate bad type vars more than once they could potentially
--	become different types, which makes the system unsound.
--
-- It would be better to do some back-tracking to recover from this, but instead
--	we just check for wrongly generalised type vars each time we do an
--	instantiation and report an error asking for a type sig if there are problems.
--
-- Type sigs are added to the graph before anything else, so we'll see the mutability
--	constraints during generaliation on the next time around.
-- 
module Type.Check.SchemeDanger
	( checkSchemes
	, checkSchemeDangerCid
	, checkSchemeDanger)
where
import Shared.VarPrim
import Type.Location
import Type.Exp
import Type.Error
import Type.Util
import Type.State
import Type.Class
import Shared.Error
import Util
import qualified Shared.Var	as Var
import qualified Data.Set	as Set

-----
stage	= "Type.Check.SchemeDanger"
debug	= False
trace s	= when debug $ traceM s


checkSchemeDangerCid :: ClassId -> SquidM [Error]
checkSchemeDangerCid cid
 = do	Just c	<- lookupClass cid
 	checkSchemeDanger [] c
 
checkSchemes :: SquidM ()
checkSchemes 
 = do	trace	$ ppr $ "*    checkSchemes\n"
	errs	<- foldClasses checkSchemeDanger []
	addErrors errs
	trace	$ ppr $ "*    done checking schemes\n"


-- Check that the regions above quantified dangerous vars haven't become mutable.
checkSchemeDanger :: [Error] -> Class -> SquidM [Error]
checkSchemeDanger errs c
	| Class { classKind	= kValue 
		, classId	= cid
		, classType	= Just t }	<- c
	, TForall b k x				<- t

	= do	trace 	$ "*   checkSchemeDanger\n"
			% "    t = " % t % "\n"

		let clo	= toFetterFormT
			$ trimClosureC_constrainForm
				Set.empty Set.empty 
			$ toConstrainFormT
			$ TFree (Var.new "foo") x
		
		let ds	= catMaybes
			$ map (\d -> case d of
					TFree _ (TDanger t1 t2)	-> Just (t1, t2)
					_			-> Nothing)
			$ flattenTSum clo
		
		trace	$ "    clo  = " % clo 	% "\n"
			% "    ds   = " % ds	% "\n"
			
		moreErrs	
			<- liftM catMaybes
			$ mapM (checkDanger c) ds
			
		trace	$ "    errs  = " % errs	% "\n\n"
		return (errs ++ moreErrs)

	| otherwise
	= return errs
 
	
-- Produce an error if the region is mutable
checkDanger :: Class -> (Type, Type) -> SquidM (Maybe Error)
checkDanger (Class 
		{ classId 	= cidScheme
		, classType 	= Just t })
		( tRegion@(TClass kR cidR)
		, t2)

	| kR	== kRegion
	, TVar k varT	<- t2
	= do 	mfMutableSrc	<- lookupMutable cidR
		case mfMutableSrc of
	 	 Nothing	-> return Nothing
	 	 Just (fMutable, srcMutable)
	  	  -> do
			varScheme	<- makeClassName cidScheme
			return	$ Just
				$ ErrorLateConstraint
					{ eScheme 		= (varScheme, t)
					, eMutableRegion	= tRegion
					, eMutableFetter	= fMutable
					, eMutableSrc		= srcMutable
					, eDangerVar		= varT }

	-- var is monomorphic, ok
	| kR	== kRegion
	, TClass{}	<- t2
	= 	return Nothing
	
	
checkDanger (Class { classType = t }) (t1, t2)
 = panic stage
	$ "checkDanger: no match\n"
	% "    c  = " % t	% "\n"
	% "    t1 = " % t1	% "\n"
	% "    t2 = " % t2	% "\n"
	% "\n"
	
	
-- See if there is a Mutable fetter in this class
lookupMutable 
	:: ClassId 
	-> SquidM (Maybe (Fetter, TypeSource))

lookupMutable cid
 = do	Just (Class { classFetterSources = fs_src })	
 		<- lookupClass cid

	let fs_srcs_mutable
		= [ (f, src) 	
			| (f@(FConstraint v _), src) <- fs_src
			, v == primMutable ]
				
	case fs_srcs_mutable of
	 []	-> return $ Nothing
	 (f, src) : _	
	  -> do	f'	<- updateVC f
		return $ Just (f', src)

