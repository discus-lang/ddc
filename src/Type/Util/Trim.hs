
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
import qualified Type.Util.PackFast	as PackFast
import Type.Util.Bits
import Type.Util.Kind

import Shared.Pretty
import Shared.Var			(Var)
import qualified Shared.Var		as Var
import qualified Shared.VarUtil		as Var

import qualified DDC.Base.NameSpace	as Var

import qualified Data.Set		as Set
import qualified Data.Map		as Map
import qualified Debug.Trace		as Debug

-----
stage	= "Type.Util.Trim"
debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x


-- | Trim the closure portion of this type
trimClosureT 
	:: Set Type	-- ^ variables that are quantified in this context
	-> Set Type	-- ^ primary region variables of this context
	-> Type 
	-> Type

trimClosureT quant rsData tt
  = trace ("trimming " % tt % "\n")
  $ let	tt_trimmed	= trimClosureT' quant rsData tt
	
	tt_packFast	= toFetterFormT 
			$ PackFast.packType
			$ toConstrainFormT tt_trimmed
			
	tt'		= trace ( "tt_trimmed  = " % tt_trimmed 	% "\n"
				% "tt_packFast = " % tt_packFast	% "\n\n")
		 		$ tt_packFast
		
    in	if tt' == tt
    		then tt'
		else trimClosureT quant rsData tt'

trimClosureT' quant rsData tt		
 = case tt of
 	TFetters t fs
	  -> let fs'	= catMaybes 
	  		$ map (trimClosureT_fs quant rsData) fs
	     in  addFetters fs' t

	TConstrain tBody crs@(Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.mapWithKey (trimClosureT_tt quant rsData) crsEq
		crsMore'	= Map.mapWithKey (trimClosureT_tt quant rsData) crsMore
		crs'		= Constraints crsEq' crsMore' crsOther
	    in	addConstraints crs' tBody

	_	-> tt


-- | Trim a closure down to its interesting parts
trimClosureC :: Set Type -> Set Type -> Closure -> Closure
trimClosureC quant rsData cc
 = let 	cc_trimmed	= trimClosureC' quant rsData cc

	cc_packed	= toFetterFormT 
			$ PackFast.packType 
			$ toConstrainFormT
			$ cc_trimmed
			
	cc'		= trace 
 				( "trimClosureC\n"	
 				% "    rsData = " % rsData	% "\n"
				% "    cc     = " % cc		% "\n") 
				cc_packed
   in	if cc' == cc
   	 then cc'
	 else trimClosureC quant rsData cc'
	
trimClosureC' quant rsData cc
 = let down	= trimClosureC quant rsData
   in  case cc of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar k v
		| Set.member cc quant 	
		-> TBot kClosure

		| otherwise		
		-> makeTSum kClosure
			$ (cc : [TDanger r cc	| r	<- Set.toList rsData
						, r /= cc])

	-- cids are never quantified so we always have to keep them.
	TClass k _	
		-> makeTSum kClosure
			$ cc : [TDanger r cc	| r	<- Set.toList rsData
						, r /= cc]

	TBot k	-> cc
	TTop k	-> cc

	-- Trim all the elements of a sum
	TSum k cs	
		-> makeTSum kClosure 
		$  map down
		$  flattenTSum cc

	TFetters c fs
	 -> addFetters 
	 	(catMaybes $ map (trimClosureC_fs quant rsData) fs) 
	 	(down c)

	TConstrain t crs@Constraints { crsEq, crsMore, crsOther }
	 -> let	t'		= trimClosureC quant rsData t
		crsEq'		= Map.mapWithKey (\t1 t2 -> trimClosureT quant rsData t2) crsEq
		crsMore'	= Map.mapWithKey (\t1 t2 -> trimClosureT quant rsData t2) crsEq
	    in	addConstraints (Constraints crsEq' crsMore' crsOther) t'

	-- add quantified vars to the set
	TForall b k t		
	 -> let quant'	= Set.insert (TVar k (varOfBind b)) quant
	    in	trimClosureC quant' rsData t

	-- free
	TFree v1 (TDanger t1 t2)
	 | kindOfType_orDie t1 == kRegion
	 , kindOfType_orDie t2 == kRegion
	 -> makeTSum kClosure
	 	[ TFree v1 t1
		, TFree v1 t2]

	     
	TFree tag (TDanger t1 (TDanger t2 t3))
	 -> makeTSum kClosure
	 	[ TFree tag (TDanger t1 t2)
		, TFree tag (TDanger t1 t3)
		, TFree tag (TDanger t2 t3) ]

	TFree tag (TDanger t1 t2)
	 -> if kindOfType_orDie t2 == kClosure
		then TFree tag 
			  $ makeTDanger tag t1 (down t2)
		else makeTSum kClosure
			  $ map (TFree tag)
			  $ map (makeTDanger tag t1)
			  $ trimClosureC_t tag quant rsData t2


	TFree tag t
	 -> if kindOfType_orDie t == kClosure
		then TFree tag 	$ down t
		else TFree tag 	$ makeTSum kClosure 
				$ trimClosureC_t tag quant rsData t

	TDanger{}
	 -> cc

	_ -> panic stage
		$ "trimClosureC: no match for " % show cc



-- | Trim a value type element of a closure.
trimClosureC_t :: Var -> Set Type -> Set Type -> Type -> [Type]
trimClosureC_t tag quant rsData tt
 = let tsBits	= trimClosureC_t' tag quant rsData tt
   in  {- trace 	( "trimClosureC_t " % tt % "\n"
   		% "    rsData: " %> rsData	% "\n"
   		% "    tsBits: " %> tsBits	% "\n")	$  -}
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

	TConstrain tBody crs@(Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsEq
		crsMore'	= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsMore
		crs		= Constraints crsEq' crsMore' crsOther
	    	cBits		= down tBody
	    in	map (addConstraints crs) cBits
			
	-- Trim under foralls
	TForall b k t		
	 -> let	quant'	= Set.insert (TVar k (varOfBind b)) quant
	    in	trimClosureC_t tag quant' rsData t
	
	TSum k ts	-> catMap down ts

	TCon{}		-> []
	TBot{}		-> []
	TTop{}		-> [tt]

	-- when we enter into a data object remember that we're under its primary region.
	TApp{}
	 -> let result
		 | Just (v, k, [])	<- takeTData tt 
		 = []
		
		
		 | Just (v, k, (t:ts))	<- takeTData tt
		 = if kindOfType_orDie t == kRegion 
		   then let 	rsData'	= Set.insert t rsData
				vs	= freeVars (t:ts)
			in  	catMap (trimClosureC_t tag quant rsData') (t:ts)
				 ++ map (TDanger t) [TVar (kindOfSpace $ Var.nameSpace v) v
							| v <- Set.toList vs 
							, not $ Var.isCtorName v]
		   else catMap down ts

		  | Just (t1, t2, eff, clo) <- takeTFun tt
		  = down clo
		
		  | otherwise
		  = []
	    in	result
	
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
	| kindOfType_orDie t == kRegion
	= TFree tag t
	
	| otherwise	= TDanger r t


-- | Trim a fetter of a closure
trimClosureC_fs :: Set Type -> Set Type -> Fetter -> Maybe Fetter
trimClosureC_fs quant rsData ff
 = case ff of
 	FWhere c1 c2	

	 -- more closure information
	 |  kindOfType_orDie c1 == kClosure
	 -> Just $ FWhere c1 $ trimClosureC quant rsData c2

	 -- effect information might be referenced in a type constructor
	 | kindOfType_orDie c1 == kEffect
	 -> Just $ FWhere c1 c2

	FMore c1 c2
	 | kindOfType_orDie c1 == kClosure
	 -> Just $ FMore c1 $ trimClosureC quant rsData c2

	 | kindOfType_orDie c1 == kEffect
	 -> Just $ FMore c1 c2

	_ -> Nothing


trimClosureC_tt 
	:: Set Type 
	-> Set Type 
	-> (Type, Type)
	-> Maybe (Type, Type)

trimClosureC_tt quant rsData (c1, c2)
 	| kindOfType_orDie c1 == kClosure
	= Just (c1, trimClosureC quant rsData c2)
	
	| kindOfType_orDie c1 == kEffect
	= Just (c1, c2)
	
	| otherwise
	= Nothing
	

-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_fs :: Set Type -> Set Type -> Fetter -> Maybe Fetter
trimClosureT_fs quant rsData ff
 = case ff of
 	FWhere c1 c2	
	 |  kindOfType_orDie c1 == kClosure
	 -> Just $ FWhere c1 $ trimClosureC quant rsData c2

	FMore c1 c2
	 | kindOfType_orDie c1 == kClosure
	 -> Just $ FMore c1 $ trimClosureC quant rsData c2

	_ -> Just ff


trimClosureT_tt 
	:: Set Type
	-> Set Type
	-> Type -> Type
	-> Type

trimClosureT_tt quant rsData c1 c2
	| kindOfType_orDie c1 == kClosure
	= trimClosureC quant rsData c2
	
	| otherwise
	= c2
