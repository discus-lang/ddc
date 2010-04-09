
-- | Utils concerning Globs.
module Core.Glob
	( Glob(..)
	, globEmpty
	, globOfTree
	, treeOfGlob
	, seqOfGlob
	, bindingArityFromGlob 
	, typeFromGlob
	, varIsBoundAtTopLevelInGlob
	, mapToTopsWithExpsOfGlobM
	, mapBindsOfGlob
	, mapBindsOfGlobM
	, mapBindsOfGlobM_)
where
import Core.Exp
import Core.OpType
import Core.Util.Slurp
import Type.Exp
import Type.Util
import Data.Maybe
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import Data.Map			(Map)
import Data.Set			(Set)
import Data.Sequence		(Seq, (><))
import Data.Foldable		(foldr)
import Data.Traversable		(mapM)
import Control.Monad		hiding (mapM)
import Prelude			hiding (foldr, mapM)
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Util.Data.Map	as Map
import qualified Data.Sequence	as Seq
import {-# SOURCE #-} Core.Reconstruct


stage	= "Core.Glob"

-- | A Glob provides a fast way to locate particular top level declarations.
--   Note: Don't add extra fields to this type that can't be reconstructed
-- 	   directly from a Tree. We want to be able to convert between
--	   Glob and Tree without losing information.	      
data Glob
	= Glob
	{ globClass		:: Map Var Top
	, globEffect		:: Map Var Top
	, globRegion		:: Map Var Top
	, globExternData	:: Map Var Top
	, globExtern		:: Map Var Top

	, globData		:: Map Var Top
	
	-- | Map of data constructor name definition,
	--	for all data constructors from all data types.
	, globDataCtors		:: Map Var CtorDef

	-- | Data class dictionary declarations.
	, globClassDict		:: Map Var Top

	-- | Set of value variables that are overloaded as they are methods
	--	in some type class.
	, globClassMethods 	:: Set Var
		
	-- | Map of class name -> instances for that class.
	, globClassInst		:: Map Var (Seq Top)

	, globBind		:: Map Var Top
	}
	deriving Show


-- | An empty Glob.
globEmpty :: Glob
globEmpty
 	= Glob
	{ globClass		= Map.empty
	, globEffect		= Map.empty
	, globRegion		= Map.empty
	, globExternData	= Map.empty
	, globExtern		= Map.empty
	, globData		= Map.empty
	, globDataCtors		= Map.empty
	, globClassDict		= Map.empty
	, globClassMethods	= Set.empty
	, globClassInst		= Map.empty
	, globBind		= Map.empty }
	

-- | Convert a program Tree to a Glob.
globOfTree :: Tree -> Glob
globOfTree ps
 = let	-- Add all the tops to the glob.
	globTops	= foldr insertTopInGlob globEmpty ps
	
	-- Build the map of data constructors.
 	ctors		= Map.unions $ [ topDataCtors p | p@PData{} <- ps ]
	globTops_ctors	= globTops { globDataCtors = ctors }

	-- Build the set of vars defined at top level.
	vsMethods	= Set.unions
			$ map (Set.fromList . (map fst))
			$ map topClassDictTypes
			$ Map.elems 
			$ globClassDict globTops
			
	globTops_classMethods
			= globTops_ctors 
			{ globClassMethods = vsMethods }

   in	globTops_classMethods


-- | Insert a top into a glob. 
--	If the top is already there the old one is updated.
insertTopInGlob :: Top -> Glob -> Glob
insertTopInGlob pp glob
 = case pp of
	PClass{}	
	 -> glob { globClass 		
			= Map.insert (topClassName pp)		pp (globClass glob) }

	PEffect{}	
	 -> glob { globEffect 		
			= Map.insert (topEffectName pp)		pp (globEffect glob) }

	PRegion{}	
	 -> glob { globRegion 		
			= Map.insert (topRegionName pp)		pp (globRegion glob) }
	
	PExternData{}
	 -> glob { globExternData 	
			= Map.insert (topExternDataName pp)	pp (globExternData glob) }
	
	PExtern{}	
	 -> glob { globExtern 		
			= Map.insert (topExternName pp) 	pp (globExtern glob) }

	PData{}	
	 -> glob { globData 		
			= Map.insert (topDataName pp) 		pp (globData glob) }

	PClassDict{}	
	 -> glob { globClassDict	
			= Map.insert (topClassDictName pp) 	pp (globClassDict glob) }

	PClassInst{}	
	 -> glob { globClassInst	
			= Map.adjustWithDefault 
				(Seq.|> pp) Seq.empty 
				(topClassInstName pp)
				(globClassInst glob) }

	PBind{}	
	 -> glob { globBind		
			= Map.insert (topBindName pp)	 	pp (globBind glob) }


-- | Convert a `Glob` back to a `Tree`.
treeOfGlob :: Glob -> Tree
treeOfGlob glob
 	= (Map.elems
		$ Map.unions
		[ globClass  		glob 
		, globEffect		glob
		, globRegion		glob
		, globExternData	glob
		, globExtern		glob
		, globData		glob
		, globClassDict		glob
		, globBind		glob ])
	++ (foldr (:) [] $ join $ Seq.fromList $ Map.elems $ globClassInst glob)
	


-- | Convert a `Glob` to a sequence of `Top`s.
seqOfGlob :: Glob -> Seq Top
seqOfGlob glob
	=   seqOfMap (globClass		glob)
	><  seqOfMap (globEffect	glob)
	><  seqOfMap (globRegion	glob)
	><  seqOfMap (globExternData	glob)
	><  seqOfMap (globExtern	glob)
	><  seqOfMap (globData		glob)
	><  seqOfMap (globClassDict	glob)
	><  (join $ Seq.fromList $ Map.elems $ globClassInst glob)
	><  seqOfMap (globBind		glob)
	where seqOfMap m = Map.fold (Seq.<|) Seq.empty m

	
-- | If this glob has a value binding, then get its binding arity.
--	The "binding arity" is the number of args directly accepted
--	by the function, ie the number of outermost value lambdas.
bindingArityFromGlob :: Var -> Glob -> Maybe Int
bindingArityFromGlob v glob
	| Just pp@PExtern{}	<- Map.lookup v (globExtern glob)
	= let	tOperational	= topExternOpType pp
	  in	Just $ (length $ flattenFun tOperational) - 1

	| Just pp@PBind{}	<- Map.lookup v (globBind glob)
	= let	tOperational	= superOpTypeX $ topBindExp pp
	  in 	Just $ (length $ flattenFun tOperational) - 1

	| Just ctor@CtorDef{}	<- Map.lookup v (globDataCtors glob)
	= Just $ ctorDefArity ctor
	
	| otherwise
	= Nothing


-- | Get the type of some top-level thing.
typeFromGlob :: Var -> Glob -> Maybe Type
typeFromGlob v glob
	-- External decls are already annotated with their types.
	| Just pp@PExtern{}	<- Map.lookup v (globExtern glob)
	= Just $ topExternType pp
	
	-- If we can slurp out the type directly from its annots then use that, 
	--	otherwise we'll have to reconstruct it manually.
	| Just pp@PBind{}	<- Map.lookup v (globBind glob)
	= Just $ fromMaybe (reconX_type "Core.Glob.typeFromGlob" (topBindExp pp))
			   (maybeSlurpTypeX (topBindExp pp))
		
	| Just ctor@CtorDef{}	<- Map.lookup v (globDataCtors glob)
	= Just $ ctorDefType ctor

	| otherwise
	= Nothing


-- | Check if a value variable is bound at top-level in this `Glob`.
--	Since we know it is a value var we don't have to check 
varIsBoundAtTopLevelInGlob :: Glob -> Var -> Bool
varIsBoundAtTopLevelInGlob glob v
 = case varNameSpace v of
	NameValue
	 -> or	[ Map.member v $ globExtern	glob
		, Map.member v $ globDataCtors	glob
		, Map.member v $ globBind	glob
		, Set.member v $ globClassMethods glob ]
	
	NameType
	 -> or	[ Map.member v $ globExternData glob
		, Map.member v $ globData	glob ]
		
	NameRegion
	 ->	Map.member v $ globRegion	glob 
	
	NameEffect
	 ->	Map.member v $ globEffect	glob
	
	NameClosure -> False
			
	_ -> panic stage 
	   $ "varIsBoundAtTopLevelInGlob: not implemented for " % show (varNameSpace v)
		

-- As often want to do something to all the top level bindings in a glob,
--	we define some useful transforms...

-- | Apply a monadic computation to all the bindings in a `Glob` that contain value Exps.
--	This is the globBinds as well as the globClassInsts
mapToTopsWithExpsOfGlobM
	:: Monad m
	=> (Top -> m Top)
	-> Glob -> m Glob
		
mapToTopsWithExpsOfGlobM f glob
 = do	psBind'		<- mapM f 	 $ globBind glob
	psClassInsts'	<- mapM (mapM f) $ globClassInst glob
	return	$ glob 	{ globBind 	= psBind'
			, globClassInst	= psClassInsts' }


-- | Apply a function to all PBinds in a `Glob`.
mapBindsOfGlob :: (Top -> Top) -> Glob -> Glob
mapBindsOfGlob f 
	= liftToBindsOfGlob (fmap f)


-- | Apply a monadic computation to all the PBinds of a `Glob`.
mapBindsOfGlobM :: Monad m => (Top -> m Top) -> Glob -> m Glob
mapBindsOfGlobM f 
	= liftToBindsOfGlobM (mapM f)


-- | Apply a monadic computation to all the PBinds of a `Glob`, discarding the result.
mapBindsOfGlobM_ :: Monad m => (Top -> m a) -> Glob -> m ()
mapBindsOfGlobM_ f glob
 	= mapM_ f $ Map.elems $ globBind glob
	

-- These aren't exported, as we don't have any reason to use the 'Map' versions yet.
-- | Apply a function to the binding map of a `Glob`.
liftToBindsOfGlob 
	:: (Map Var Top -> Map Var Top) 
	-> Glob -> Glob

liftToBindsOfGlob f glob
	= glob { globBind = f (globBind glob) }


-- | Apply a monadic computation to the binding map of a `Glob`.
liftToBindsOfGlobM 
	:: Monad m
	=> (Map Var Top -> m (Map Var Top))
	-> Glob -> m Glob
	
liftToBindsOfGlobM f glob
 = do	binds'	<- f $ globBind glob
	return	$ glob { globBind = binds' }

