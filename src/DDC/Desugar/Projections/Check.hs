
-- | Check for problems with projection dictionaries.
module DDC.Desugar.Projections.Check
	( checkForRedefDataField
	, checkForRedefClassInst
	, checkSBindFunc)
where
import DDC.Desugar.Projections.Base
import DDC.Desugar.Exp
import DDC.Source.Error
import DDC.Type
import DDC.Type.Data
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util

-- | Check for projection names that collide with field names of the Data
--   definition they are projecting over. Log any that are found as errors.
checkForRedefDataField 
	:: Map Var DataDef
	-> Top Annot
	-> ProjectM ()

checkForRedefDataField dataMap (PProjDict _ tt ss)
 | Just (pvar, _, _)	<- takeTData tt
 = case Map.lookup pvar dataMap of
 	Nothing 	-> return ()
	Just dname	-> mapM_ (checkSBindFunc dname pvar (fieldNamesOfDataDef dname)) ss

checkForRedefDataField _ _ = return ()


-- | Check for redefined instances.
checkForRedefClassInst
	:: Map String Var
	-> Top Annot
	-> ProjectM (Map String Var)

checkForRedefClassInst map ci@(PClassInst sp v tl@(TApp (TCon tc) _ : _) _)
 = do	let key = varName v ++ " " ++ varName (tyConName tc)
	case Map.lookup key map of
          Nothing	-> return $ Map.insert key (tyConName tc) map
	  Just redef	
	   -> do addError $ ErrorRedefClassInst v redef (tyConName tc)
                 return map

checkForRedefClassInst map _  
	= return map


checkSBindFunc 
	:: DataDef
	-> Var
	-> Map String Var
	-> Stmt Annot
	-> ProjectM ()
checkSBindFunc def pvar dfmap (SBind _ (Just v) _)
 = case Map.lookup (varName v) dfmap of
	Nothing		-> return ()
	Just redef	
		-> addError 
		$ ErrorProjectRedefDataField redef pvar 
		$ dataDefName def

checkSBindFunc _ _ _ _ 
	= return ()


fieldNamesOfDataDef :: DataDef -> Map String Var
fieldNamesOfDataDef dataDef
	= Map.fromList
	$ [ (varName d, d)
		| d <- Set.toList $ fieldsOfDataDef dataDef ]
