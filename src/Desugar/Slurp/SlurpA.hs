
module Desugar.Slurp.SlurpA 
	(slurpA)

where

-----
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))
import Shared.Exp

import Desugar.Slurp.Base
import Desugar.Slurp.SlurpX

import Type.Util.Substitute
import Type.Location

-----
stage	= "Desugar.Slurp.SlurpA"

-- Alt ---------------------------------------------------------------------------------------------
slurpA	:: Alt Annot1	
	-> CSlurpM 
		( Type					-- type constraint placed on the case object.
		, Type					-- type of the RHS.
		, Effect				-- effect of evaluating the alternative.
		, Closure				-- closure of this alternative.
		, Alt Annot2				-- annotated Alt.
		, CTree)				-- constraints.

slurpA	alt@(AAlt sp gs x)
 = do
	tGuards	<- newTVarDS "guards"
	eAlt	<- newTVarES "alt"
	cAlt	<- newTVarCS "alt"

	-- slurp the guards
	--	we need to do this before slurping the RHS
	--	because slurpW sets the bindMode for any matched vars.
	--
	(cbindssGuards, mtsGuards, esGuards, csGuards, gs', qssGuards)
			<- liftM unzip6 $ mapM slurpG gs

	-- slurp the RHS of the alternative
	(tX, eX, cX, x', qsX)
			<- slurpX x

	let vsBoundV	= map fst $ concat cbindssGuards
--	let tsBoundT	= map (TVar KData) $ map snd $ concat cbindssGuards

	let qs	=
		[ CEqs (TSU $ SUGuards sp) (tGuards : catMaybes mtsGuards)
		, CEq  (TSE $ SEGuards sp) eAlt	  $ makeTSum   KEffect  (eX : esGuards)
		, CEq  (TSC $ SCGuards sp) cAlt   $ makeTMask  KClosure 
							(makeTSum KClosure (cX : csGuards))
							(makeTSum KClosure 
								$ map TTag vsBoundV) ]

	let cbindsGuards	= concat cbindssGuards
	let bind 
		| []	<- cbindsGuards
		= BNil
		
		| otherwise
		= BDecon [vDeconT | (vDeconV, vDeconT) <- cbindsGuards]

	return	( tGuards
		, tX
		, eAlt
		, cAlt
		, AAlt Nothing gs' x'
		, CBranch
			{ branchBind	= bind
			, branchSub	= qs ++ (concat qssGuards) ++ qsX})


-- Guards ------------------------------------------------------------------------------------------
slurpG 	:: Guard Annot1
	-> CSlurpM 
		( [(Var, Var)]		-- (value var, type var) of vars bound by the guard.
		, Maybe Type		-- type constraint placed on case object.
		, Effect		-- effect of evaluting the guard.
		, Closure		-- closure of guard.
		, Guard Annot2		-- annotated guard.
		, [CTree])		-- constraints.
		
slurpG	(GCase sp w)
 = do	
 	(cbindsW, tW, w', qsW)	
 		<- slurpW w

 	return	( cbindsW
		, Just tW
		, pure
		, empty
		, GCase Nothing w'
		, qsW )
		
 	
slurpG	(GExp sp w x)
 = do
	eGuard	<- newTVarES "guard"
	cGuard	<- newTVarCS "guard"

	(cbindsW, tW, w', qsW) 
		<- slurpW w
		
	-- slurp the matched exp
	(tX, eX, cX, x', qsX)
		<- slurpX x

	let vsBoundV	= map fst cbindsW

	-- make the effect of testing the case object
	let effMatch
		= case w of
			-- if the LHS is a plain var there is no effect
			WVar{}	-> []

			-- otherwise we need to read the primary region
			_	-> [TEffect primReadH [tX]]

	let qs = 
		[ CEq	(TSU $ SUGuards sp)	tX	$ tW 
		, CEq	(TSE $ SEGuardObj sp)	eGuard	$ makeTSum KEffect (eX : effMatch)
		, CEq	(TSC $ SCGuards sp)	cGuard	$ makeTMask 
							KClosure 
							cX
							(makeTSum KClosure (map TTag vsBoundV)) ]

	-----	
	return	( cbindsW		
		, Nothing
		, eGuard
		, cGuard
		, GExp Nothing w' x'
		, qsW ++ qsX ++ qs )
		

-- Pat ---------------------------------------------------------------------------------------------
slurpW	:: Pat Annot1
	-> CSlurpM		
		( [(Var, Var)]		-- (value var, type var) of vars bound by pattern.
		, Type			-- type of the pattern.
		, Pat Annot2		-- annotatted pattern.
		, [CTree])		-- constraints for each arg in the pattern.

slurpW	(WConLabel sp vCon lvs)
 = do	
	-- Lookup what data type this constructor belongs to.
	ctorType	<- gets stateCtorType

	let tData@(TData _ tsData)
		= case Map.lookup vCon ctorType of
			Nothing -> panic stage 
				$ "slurpW: can't find type data type definition for data constructor '" 
				% vCon % "'"

			Just x	-> x


	-- Instantiate each of the poly-vars in the data type.
	let vsData	= map (\(TVar k v) -> v) tsData

	vsInst		<- mapM newVarZ vsData
	let tsInst	=  map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vsInst


	-- Apply the substitution to the data type.
	let subInst	= Map.fromList 
			$ zip tsData tsInst

	let tPat	= subTT_noLoops subInst tData

	-- Slurp constraints for each of the bound fields.
	(lvs', cBinds, cTrees)
			<- liftM unzip3
			$  mapM (slurpLV vCon tData subInst) lvs
			
 	return	( catMaybes cBinds
		, tPat
		, WConLabel Nothing vCon lvs'
		, (concat cTrees) )
		

slurpW	(WConst sp c)
 = do
	tD@(TVar KData tV)	<- newTVarDS "const"
	tE			<- newTVarES "const"
	tConst			<- getConstType c

	wantTypeV tV

 	return	( []
		, tD
		, WConst (Just (tD, tE)) c
		, [CEq (TSV $ SVLiteralMatch sp c) tD tConst])

slurpW	(WVar sp v)
 = do	Just tBound@(TVar k vT)	<- bindVtoT v
 			
	return	( [(v, vT)]
		, tBound
		, WVar (Just (tBound, pure)) v
		, [])
	
slurpW w
 = panic stage $ "slurpW: no match for " % show w % "\n"


-- Labels ------------------------------------------------------------------------------------------
slurpLV	:: Var				-- Constructor name.
	-> Type				-- Return type of constructor.
	-> Map Type Type		-- Instantiation of data type vars.
	-> (Label Annot1, Var)
	-> CSlurpM 
		( (Label Annot2, Var)
		, Maybe (Var, Var)
		, [CTree] )
	
slurpLV vCon tData subInst (LIndex sp ix, v)
 = do	
	-- create a new type var for this arg.
 	Just (TVar KData vT)	<- bindVtoT v

	-- Lookup the fields for this constructor.
	ctorFields	<- gets stateCtorFields
	let Just fields	= Map.lookup vCon ctorFields
	
	-- Get the field that this label is referring to.
	let mField	= lookup ix $ zip [0..] 
			$ [ f	| f <- fields
				, dPrimary f ]

	wantTypeV vT	


	case mField of
	 -- Ctor has required field, ok.
	 Just field
	  -> do	let tField	= subTT_noLoops subInst
				$ dType field

		return	( (LIndex Nothing ix, v)
			, Just (v, vT)
			, [CEq (TSV $ SVMatchCtorArg sp) (TVar KData vT) tField] )

         -- Uh oh, there's no field with this index.
	 --	Add the error to the monad and return some dummy info so
	 --	that the slurper can proceed and maybe find more errors.
	 Nothing
	  -> do addError
	   	  (ErrorCtorAirity 
			{ eCtorVar		= vCon
			, eCtorAirity		= length fields
			, ePatternAirity	= ix + 1})

		let v	= Var.new "<error>"

		return	( (LVar Nothing v, v)
			, Nothing
			, [] )



slurpLV vCon tData subInst (LVar sp vField, v)
 = do
 	-- Create a new type var for this arg.
 	Just (TVar KData vT)	<- bindVtoT v
 
 	-- Lookup the fields for this constructor.
 	ctorFields	<- gets stateCtorFields
	let Just fields	= Map.lookup vCon ctorFields
	
	-- Get the type for this field.
	let tFieldCands	= [ dType f	| f	<- fields
					, liftM Var.name (dLabel f) == Just (Var.name vField) ]

	let tField_	= case tFieldCands of
				[f]	-> f
				[]	-> panic stage	$ "slurpLV: no field named " % vField % "\n"
							% "    " % Var.prettyPos vField % "\n"
							% "    vCon      = " % vCon	% "\n"
							% "    tData     = " % tData	% "\n"
							% "    fields are " % (map dLabel fields) % "\n"
					
	let tField	= subTT_noLoops subInst 
			$ tField_

 	return 	( (LVar Nothing vField, v)
 		, Just (v, vT)
		, [CEq (TSV $ SVMatchCtorArg sp) (TVar KData vT) tField] )
		




