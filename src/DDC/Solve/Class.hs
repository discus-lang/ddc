{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | A ``class`` is a member of the type graph. Many of these are regular type equivalence classes, 
--   but some are also administrative like `ClassForward`, or uses to contain constraints, like 
--   `ClassFetter`. We're storing fetters as classes because it gives them their own unique
--   classids and makes them easy to refer to.
--   
module DDC.Solve.Class
	( Class(..)
	, classEmpty)
where
import Type.Location
import DDC.Solve.Node
import DDC.Type
import DDC.Var
import Data.Sequence		(Seq)
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-- | A member of the type graph.
data Class

	-- | An unallocated class.
	= ClassUnallocated						

	-- | A reference to another class.
	--	When two classes are merged one of them is rewritten to a 
	--	forward that points to the other.
	| ClassForward 
		{ classId		:: !ClassId
		, classIdFwd		:: !ClassId }

	-- | Some auxilliary constraint between classes.
	| ClassFetter
		{ classId		:: ClassId
		, classFetter		:: Fetter 
		, classSource		:: TypeSource }

	-- | A deleted fetters.
	--   	This holds the deleted class for debugging.
	| ClassFetterDeleted 
		Class

	-- | An equivalence class. This contains a list of `Node` types, which are
	--   the primitive constraints that are contributing to the overall type of the class.
	| Class	{
		-- | A unique id for this class
		  classId		:: ClassId

		-- | The kind of the class.
		, classKind		:: Kind	

		-- | The canonical name of this class.
		--   This is taken from one of the aliases, or created fresh if none exists.
		, className		:: Maybe Var

		-- | Other names for the class.
		, classAliases		:: [(Var, TypeSource)]
		
		-- | Why this class was allocated. This can be used as an overall source for
		--   kind error messages if the classTypeSources list is empty.
		, classSource		:: TypeSource			

		-- | For types of non-injective kind, that is everything besides effect and closures,
		--   gives the result of unifying the nodes in the classTypeSources list. 
	 	--   If any constraints were recently added to the class then this will be Nothing, 
		--   signaling that we need to run the unifier on it.
		, classUnified		:: Maybe Node

		-- Type constraints contributing to this class ------------------------------------
		-- | Constraints that have been added to this class, including source information.
		--	If a type error is encountered, then this information can be used to reconstruct
		--	/why/ this particular node has the type it does.
		-- 
		--   INVARIANT: The node type is not an NVar or NBot.
		, classTypeSources	:: [(Node, TypeSource)]	 

		-- | Single parameter type class constraints on this equivalence class.
		--	Maps var on constraint (like Eq) to the source of the constraint.
		--	If a type error is encountered, then this information can be used to reconstruct
		--	/why/ this particular node has the type it does.
		, classFetters		:: Map Var (Seq TypeSource)

		-- | Multi-parameter type class constraints acting on this equivalence class.
		--	MPTC's are stored in their own ClassFetter nodes, and this list points to all
		--	the MPTC's which are constraining this class.
		, classFettersMulti	:: Set ClassId }
		deriving (Show)


-- | Make an initial, empty class.
classEmpty :: ClassId -> Kind -> TypeSource -> Class
classEmpty cid kind src
	= Class
	{ classId		= cid
	, className		= Nothing
	, classAliases		= []
	, classKind		= kind
	, classSource		= src
	, classUnified		= Nothing
	, classTypeSources	= []
	, classFetters		= Map.empty
	, classFettersMulti	= Set.empty }



