
-- | Table holding projection definitions
--   TODO: We can kill this once we've exclusively using Desugar Globs.
module DDC.Desugar.Projections.ProjTable
	( ProjTable
	, slurpProjTable)
where
import DDC.Desugar.Projections.Base
import DDC.Desugar.Exp
import DDC.Type
import DDC.Var
import Util
import qualified Data.MapUtil	as Map


-- | Slurp out all the projections in a tree into a projection table
--   for easy lookup
--	
--   BUGS: If there are multiple tables for a particular type then 
--	   later ones replace earlier ones. We should really merge
--	   the tables together here.
--
type	ProjTable	
	= Map Var [(Type, Map Var Var)]

slurpProjTable
	:: Tree Annot
	-> ProjTable

slurpProjTable tree
 = let
	-- Slurp out the projection dictionaries into maps for later use
	--	in toCore.
	projDictPs
		= [p	| p@(PProjDict _ _ _)	<- tree]
	
	projDictS s
		= case s of
			SBind _ (Just v1) (XVarInst _ v2)	-> Just (v1, v2)
			SBind _ (Just v1) (XVar     _ v2)	-> Just (v1, v2)
			_					-> Nothing

	packProjDict (PProjDict _ t ss)
	 = case takeTData t of
		Just (vCon, _, _)
		 -> (vCon, (t, Map.fromList $ catMaybes $ map projDictS ss))

	projTable	= Map.collate 
			$ map packProjDict projDictPs
 in 	projTable
