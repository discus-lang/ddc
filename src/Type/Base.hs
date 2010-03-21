
module Type.Base
	( Class (..),	classInit
	, Graph (..),	graphInit
	
	, graphSize_init
	, graphSize_inc)
where
import Type.Exp
import Type.Location
import Util
import Data.Array.IO
import Type.Pretty		()
import Shared.Var		(Var)
import qualified Data.Map	as Map
import qualified Data.Set	as Set


-- | A Node in the type graph
data Class 
	
	-- | An unallocated class
	= ClassNil						

	-- | Reference to another class.
	--	A Forward is to the resulting class when classes are merged.
	| ClassForward ClassId					

	-- | Some auxilliary constraint between classes.
	| ClassFetter
		{ classId		:: ClassId
		, classFetter		:: Fetter 
		, classSource		:: TypeSource }

	-- | An equivalence class.
	| Class
		{
		-- | A unique id for this class
		  classId		:: ClassId

		-- | The kind of this class.
		, classKind		:: Kind				

		-- | A (non-unique) name for this class.
		--	This is taken as one of the vars from the nodes list, or generated fresh if 
		--	none exists. 
		, className		:: Maybe Var

		-- | Whether this class has been quantified
		, classQuant		:: Bool

	
		-- Type constraints contributing to this class.
		-- | The type of this class (if available)
		--	If there are constraints waiting to be unified then classQueue will be 
		--	non-empty and classType will be Nothing.
		, classType		:: Maybe Type

		-- | Type constraints waiting to be unified.
		, classQueue		:: [Type]

		-- | Constraints that have been added to this class, including source information.
		--	If a type error is encountered, then this information can be used to reconstruct
		--	/why/ this particular node has the type it does.
		, classTypeSources	:: [(Type, TypeSource)]	 

		-- | Constraints that have been added to this class, including source information.
		--	If a type error is encountered, then this information can be used to reconstruct
		--	/why/ this particular node has the type it does.
		, classFetterSources	:: [(Fetter, TypeSource)]		

		-- | Multi-parameter type class constraints acting on this equivalence class.
		--	MPTC's are stored in their own ClassFetter nodes, and this list points to all
		--	the MPTC's which are constraining this node.
		, classFettersMulti	:: Set ClassId }
		deriving (Show)


classInit cid kind
	= Class
	{ classId		= cid
	, classKind		= kind
	, className		= Nothing
	, classQuant		= False

	, classType		= Nothing
	, classQueue		= []
	, classTypeSources	= []

	, classFetterSources	= []

	, classFettersMulti	= Set.empty }
	
		
-- | The Type Graph.
data Graph
	= Graph { 
		-- | The classes
		graphClass		:: IOArray ClassId Class		

		-- | Generator for new ClassIds.
		, graphClassIdGen	:: !Int					

		-- | Type Var -> ClassId Map.
		, graphVarToClassId	:: Map Var ClassId

		-- | The classes which are active, 
		--	ie waiting to be unified or crushed.
		, graphActive		:: Set ClassId }	
					

-- | Initial size of the graph.
graphSize_init	= (5000 :: Int)

-- | Size to increase the graph by when it fills up.
graphSize_inc	= (5000 :: Int)

graphInit :: IO Graph
graphInit
 = do
	class1		<- newArray (ClassId 0, ClassId graphSize_init) ClassNil
 	return	Graph
		{ graphClass		= class1
		, graphClassIdGen	= 0
		, graphVarToClassId	= Map.empty 
		, graphActive		= Set.empty }
