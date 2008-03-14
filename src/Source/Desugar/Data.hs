
-- | Elaborate data definitions.
module Source.Desugar.Data 
	(elaborateData)
where

import Type.Util.Elaborate

import Source.Pretty
import Source.Exp

import Shared.Pretty
import Shared.Base
import Shared.VarPrim
import Shared.Var		(NameSpace(..))
import qualified Shared.Var	as Var

import Util
import qualified Data.Set	as Set

import qualified Debug.Trace

-----
debug		= False
trace s xx	= if debug then Debug.Trace.trace (pprStrPlain s) xx else xx

-----
elaborateData 
	:: Monad m
	=> (NameSpace 	-> m Var)
	-> (Var		-> m Kind)
	-> Top SourcePos -> m (Top SourcePos)

elaborateData newVarN getKind 
	p@(PData sp vData vsData ctors)
 = do
	trace 	( "elaborateData\n"
		% "    in:\n" %> p	% "\n")
		$ return ()

	let ?newVarN	= newVarN
	let ?getKind	= getKind


	-- If there are no regions in the data type at all then we need to add one
	--	to serve as the primary region.
	let dontAddPrimary v
		= vData == primTUnit || Set.member v primTVarsUnboxed

	vsPrimary
		<- if    (null (filter (\v -> Var.nameSpace v == NameRegion) vsData))
		      && (not $ dontAddPrimary vData)
			then do	vPrimary	<- newVarN NameRegion
				return	[vPrimary]
			else	return	[]

	-- Add missing regions to constructor types
	(ctors', vksNew)	<- liftM unzip $ mapM elaborateCtor ctors
	let vsData'		= nub $ vsPrimary ++ vsData ++ (map fst $ concat vksNew)
	let p'			= PData sp vData vsData' ctors'

	trace	( "    out:\n"	%> p'	% "\n")
		$ return ()
		
	return p'


elaborateCtor 
	:: Monad m
	=> (?newVarN :: NameSpace 	-> m Var)
	-> (?getKind :: Var		-> m Kind)
	-> (Var, [DataField a Type])
	-> m 	( (Var, [DataField a Type])
		, [(Var, Kind)] )

elaborateCtor (var, fields)
 = do	(fields', vksNew)
 		<- liftM unzip 
		$ mapM elaborateField fields

 	return	( (var, fields')
		, concat vksNew)

elaborateField field@(DataField { dType = t })
 = do	
 	(t_elab, vksConst, vksMutable)	
 		<- elaborateRegionsT ?newVarN ?getKind t

 	return	( field { dType = t_elab }
		, vksConst ++ vksMutable )
	
