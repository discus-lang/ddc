
-- | Utils concerning Globs.
module Core.Glob
	( globOfTree
	, treeOfGlob
	, seqOfGlob
	, globDeclaresValue 
	, bindingArityFromGlob 
	, typeFromGlob )
where
import Core.Exp
import Core.Reconstruct
import Core.OpType
import Core.Util.Slurp
import Type.Exp
import Type.Util
import Data.Maybe
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Sequence	as Seq
import Data.Sequence		(Seq, (<|), (><))


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
	, globClassInst		= Map.empty
	, globBind		= Map.empty }
	

-- | Convert a program Tree to a Glob.
globOfTree :: Tree -> Glob
globOfTree ps
 = let	globTops	= foldr insertTopInGlob globEmpty ps
	
 	ctors		= Map.unions $ [ topDataCtors p | p@PData{} <- ps ]
	globTops_ctors	= globTops { globDataCtors = ctors }

   in	globTops_ctors


-- | Insert a top into a glob. 
--	If the top is already there the old one is updated.
insertTopInGlob :: Top -> Glob -> Glob
insertTopInGlob pp glob
 = case pp of
	PClass{}	
	 -> glob { globClass 		= Map.insert (topClassName pp)		pp (globClass glob) }

	PEffect{}	
	 -> glob { globEffect 		= Map.insert (topEffectName pp)		pp (globEffect glob) }

	PRegion{}	
	 -> glob { globRegion 		= Map.insert (topRegionName pp)		pp (globRegion glob) }
	
	PExternData{}
	 -> glob { globExternData 	= Map.insert (topExternDataName pp)	pp (globExternData glob) }
	
	PExtern{}	
	 -> glob { globExtern 		= Map.insert (topExternName pp) 	pp (globExtern glob) }

	PData{}	
	 -> glob { globData 		= Map.insert (topDataName pp) 		pp (globData glob) }

	PClassDict{}	
	 -> glob { globClassDict	= Map.insert (topClassDictName pp) 	pp (globClassDict glob) }

	PClassInst{}	
	 -> glob { globClassInst	= Map.insert (topClassInstName pp) 	pp (globClassInst glob) }

	PBind{}	
	 -> glob { globBind		= Map.insert (topBindName pp)	 	pp (globBind glob) }


-- | Convert a Glob back to a Tree.
treeOfGlob :: Glob -> Tree
treeOfGlob glob
 	=  Map.elems
	$  Map.unions
		[ globClass  		glob 
		, globEffect		glob
		, globRegion		glob
		, globExternData	glob
		, globExtern		glob
		, globData		glob
		, globClassDict		glob
		, globClassInst		glob
		, globBind		glob ]


-- | Convert a Glob to a sequence of tops
seqOfGlob :: Glob -> Seq Top
seqOfGlob glob
	=   seqOfMap (globClass		glob)
	><  seqOfMap (globEffect	glob)
	><  seqOfMap (globRegion	glob)
	><  seqOfMap (globExternData	glob)
	><  seqOfMap (globExtern	glob)
	><  seqOfMap (globData		glob)
	><  seqOfMap (globClassDict	glob)
	><  seqOfMap (globClassInst	glob)
	><  seqOfMap (globBind		glob)
	where seqOfMap m = Map.fold (Seq.<|) Seq.empty m


-- | Check whether a glob has a top level decl for this value.
--	It could be imported via an extern, be a data constructor, or a value binding.
globDeclaresValue :: Var -> Glob -> Bool
globDeclaresValue v glob
 	=  (Map.member v $ globExtern    glob)
	|| (Map.member v $ globDataCtors glob)
	|| (Map.member v $ globBind      glob)
	
	
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

