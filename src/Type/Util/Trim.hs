
-- | Trimming of closures
--	Inferred closure tend to contain a lot of information that isn't useful to the solver
--	or core IR. We can trim out a lot of this superfulous stuff.
--
--	We're only interested in data contructors.
--
--	We only need the closure part of functions:
--		ie   a -(%e1 $c1)> b
--		only the $c1 part can contain data.
--
--	Note: trimming under foralls, must retain quantification of some vars.
--
--		forall a %r1. a -($c1)> b
--		:- $c1 = Thing a %r1 %r2
--
--	reduce to
--		forall a %r1. Thing a %r1 %r2	

module Type.Util.Trim
	( trimClosureT 
	, trimClosureC
	, trimClosureT')
	
where

import Util
import Shared.Error
import Type.Exp
import Type.Plate
import Type.Pretty
import Type.Plate.FreeVars
import Type.Util.Pack
import Type.Util.Bits
import Type.Util.Kind

import Shared.Pretty
import qualified Shared.Var		as Var
import qualified Shared.VarSpace	as Var
import qualified Shared.VarUtil		as Var
import Shared.Var			(Var)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Trim"

debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x


-- | Trim the closure portion of this type
trimClosureT :: Set Type -> Set Type -> Type -> Type
trimClosureT quant rsData tt
  = let	tt'	= packType_noLoops $ trimClosureT' quant rsData tt
    in	if tt' == tt
    		then tt'
		else trimClosureT quant rsData tt'

trimClosureT' quant rsData tt		
 = case tt of
 	TFetters t fs
	  -> let fs'	= catMaybes 
	  		$ map (trimClosureT_fs quant rsData) fs
	     in  addFetters fs' t

	_	-> tt

	
-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_fs :: Set Type -> Set Type -> Fetter -> Maybe Fetter
trimClosureT_fs quant rsData ff
 = case ff of
 	FLet c1 c2	
	 |  kindOfType_orDie c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC quant rsData c2

	FMore c1 c2
	 | kindOfType_orDie c2 == KClosure
	 -> Just $ FMore c1 $ trimClosureC quant rsData c2

	_ -> Just ff


-- | Trim a closure down to its interesting parts
trimClosureC :: Set Type -> Set Type -> Closure -> Closure
trimClosureC quant rsData cc
 = let cc'	= trimClosureC2 quant rsData cc
   in  trace 
 	( "trimClosureC\n"	
 	% "    rsData = " % rsData	% "\n"
	% "    cc     = " % cc		% "\n"
	% "    cc'    = " % cc'		% "\n")
 $ trimClosureC2 quant rsData cc

trimClosureC2 quant rsData cc
 | KClosure	<- kindOfType_orDie cc
  = let cc'	= packClosure_noLoops $ trimClosureC' quant rsData cc
    in  if cc' == cc
   	 then cc'
	 else trimClosureC quant rsData cc'

 | otherwise
 = panic stage
 	$ "trimClosureC: not a closure"
	% "    cc = " % cc 


trimClosureC' quant rsData cc
 = let down	= trimClosureC quant rsData
   in  case cc of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar KClosure v
		| Set.member cc quant 	-> TBot KClosure
		| otherwise		
		-> makeTSum KClosure 
			$ (cc : [TDanger r cc	| r	<- Set.toList rsData
						, r /= cc])

	-- cids are never quantified so we always have to keep them.
	TClass KClosure _	
		-> makeTSum KClosure
			$ cc : [TDanger r cc	| r	<- Set.toList rsData
						, r /= cc]

	TBot  KClosure		-> cc
	TTop  KClosure 		-> cc

	-- Trim all the elements of a sum
	TSum  KClosure cs	
		-> makeTSum KClosure 
		$  map down
		$  flattenTSum cc

	TMask KClosure t1 t2	
	 -> let	t1'	= down t1
	    in	TMask KClosure t1' t2

	TFetters c fs
	 -> addFetters 
	 	(catMaybes $ map (trimClosureC_fs quant rsData) fs) 
	 	(down c)

	-- add quantified vars to the set
	TForall b k t		
	 -> let quant'	= Set.insert (TVar k (varOfBind b)) quant
	    in	trimClosureC quant' rsData t

	-- free
	TFree v1 (TDanger t1 t2)
	 | kindOfType_orDie t1 == KRegion
	 , kindOfType_orDie t2 == KRegion
	 -> makeTSum KClosure
	 	[ TFree v1 t1
		, TFree v1 t2]

	     
	TFree tag (TDanger t1 (TDanger t2 t3))
	 -> makeTSum KClosure
	 	[ TFree tag (TDanger t1 t2)
		, TFree tag (TDanger t1 t3)
		, TFree tag (TDanger t2 t3) ]

	TFree tag (TDanger t1 t2)
	 -> case kindOfType_orDie t2 of
	 	KClosure -> TFree tag 
			  $ makeTDanger tag t1 (down t2)

		_	 -> makeTSum KClosure
			  $ map (TFree tag)
			  $ map (makeTDanger tag t1)
			  $ trimClosureC_t tag quant rsData t2


	TFree tag t
	 -> case kindOfType_orDie t of
	 	KClosure -> TFree tag 	$ down t
		_ 	 -> TFree tag 	$ makeTSum KClosure 
					$ trimClosureC_t tag quant rsData t

	-- hrmm. this shouln't be needed
	TTag{}			-> cc

	_ -> panic stage
		$ "trimClosureC: no match for " % show cc



-- | Trim a value type element of a closure.
trimClosureC_t :: Var -> Set Type -> Set Type -> Type -> [Type]
trimClosureC_t tag quant rsData tt
 = let tsBits	= trimClosureC_t' tag quant rsData tt
   in  trace 	( "trimClosureC_t " % tt % "\n"
   		% "    rsData: " %> rsData	% "\n"
   		% "    tsBits: " %> tsBits	% "\n")	$  
		tsBits
 
trimClosureC_t' tag quant rsData tt
 = let down	= trimClosureC_t tag quant rsData
   in  case tt of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar k v
		| Set.member tt quant	-> []
		
		| otherwise		
		-> makeFreeDanger tag rsData tt

	-- classids are never quantified, so we always have to keep them.
	TClass{} 	
		-> makeFreeDanger tag rsData tt
		

	-- Trim the fetters of this data
 	TFetters c fs
	 -> let	fs'	= catMaybes $ map (trimClosureC_fs quant rsData) fs
	    	cBits	= down c
	    in	map (addFetters fs') cBits
	    
	-- Trim under foralls
	TForall b k t		
	 -> let	quant'	= Set.insert (TVar k (varOfBind b)) quant
	    in	trimClosureC_t tag quant' rsData t
	
	TSum k ts	-> catMap down ts
	TMask k t1 t2	-> [TMask k (makeTSum k $ down t1) t2]


	TBot{}		-> []
	TTop{}		-> [tt]

	-- when we enter into a data object remember that we're under its primary region.
	TApp{}
	 -> case takeTData tt of
	 	Just (v, k, [])		-> []
		Just (v, k, (t:ts))
		 	| kindOfType_orDie t == KRegion
			-> let rsData'	= Set.insert t rsData
			   in  catMap (trimClosureC_t tag quant rsData') (t:ts)
		   
			| otherwise
			-> catMap down ts
		_			-> [tt]
	
	
	TData k v []	-> []
	TData k v (t:ts)
	 	| kindOfType_orDie t == KRegion
		-> let rsData'	= Set.insert t rsData
		   in  catMap (trimClosureC_t tag quant rsData') (t:ts)
		   
		| otherwise
		-> catMap down ts
	
	    
	-- Only the closure portion of a function actually holds data
	TFun t1 t2 eff clo	-> down clo

	TEffect{}		-> []
	TFree v t		-> [trimClosureC quant rsData tt]

	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"

makeFreeDanger tag rsData t
	| Set.null rsData	= [t]

	| otherwise		
	= map (\r -> makeTDanger tag r t) 
		$ Set.toList rsData

makeTDanger tag r t
	| kindOfType_orDie t == KRegion
	= TFree tag t
	
	| otherwise	= TDanger r t


-- | Trim a fetter of a closure
trimClosureC_fs :: Set Type -> Set Type -> Fetter -> Maybe Fetter
trimClosureC_fs quant rsData ff
 = case ff of
 	FLet c1 c2	

	 -- more closure information
	 |  kindOfType_orDie c2 == KClosure
	 -> Just $ FLet c1 $ trimClosureC quant rsData c2

	 -- effect information might be referenced in a type constructor
	 | kindOfType_orDie c1 == KEffect
	 -> Just $ FLet c1 c2

	FMore c1 c2
	 | kindOfType_orDie c2 == KClosure
	 -> Just $ FMore c1 $ trimClosureC quant rsData c2

	 | kindOfType_orDie c2 == KEffect
	 -> Just $ FMore c1 c2

	_ -> Nothing

