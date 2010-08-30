
module Desugar.Slurp.Util
	( traceM
	, newVarN
	, newVarV, newVarVS
	, newTVarD, newTVarDS
	, newTVarR, newTVarRS
	, newTVarE, newTVarES
	, newTVarC, newTVarCS
	, newVarZ
	, bindVtoT
	, lbindVtoT
	, getVtoT
	, addDataDef
	, addDef
	, addError 
	, wantTypeV
	, wantTypeVs )
where
import Util
import Desugar.Slurp.State
import DDC.Solve.Error
import DDC.Desugar.Exp
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Type.Data
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Desugar.Slurp.Util"


-- Debugging --------------------------------------------------------------------------------------
traceM :: String -> CSlurpM ()
traceM	  ss
 	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
	

-- Variables --------------------------------------------------------------------------------------
newVarN :: NameSpace -> CSlurpM Var
newVarN space	= newVarNS space ""

newVarNS :: NameSpace -> String -> CSlurpM Var
newVarNS space str	
 = do	gen		<- gets stateGen
 	let spaceGen	= fromMaybe 	(panic stage $ "newVarNS: no space gen for " % show space % "\n")
					(Map.lookup space gen)

	let postfix	= if str == []
				then []
				else "" ++ str
			
	let var'	= (varWithName (pprStrPlain spaceGen ++ postfix))
			{ varId		= spaceGen 
			, varNameSpace		= space }
		
	let spaceGen'	= incVarId spaceGen
		
	modify (\s -> s { 
		stateGen	= Map.insert space spaceGen' (stateGen s)})
	
	return var'


newVarZ :: Var -> CSlurpM Var
newVarZ	var 
	= newVarN (varNameSpace var) 


bindVtoT :: Var -> CSlurpM (Maybe Type)
bindVtoT varV
 = do
	varType		<- gets stateVarType
	
	-- Check that this var isn't already bound.
	let mVarT :: Maybe Var	= Map.lookup varV varType

	case mVarT of
	 Just vBound
	  -> freakout stage 
	  	( "bindVtoT: var '" % varV % "' is already bound\n"
		% "    trying to bind " % show varV 	% "\n\n"
		% "    conflicts with " % show vBound	% "\n\n")
	
		$ return Nothing
		
	 Nothing 
	  -> do
		-- Make the new type var
		varT		<- newVarN NameType 
		let varT'	= varT 	{ varName	= varName varV
					, varInfo	= [IValueVar varV] }
		
		-- 
		modify (\s -> s { 
			stateVarType	= Map.insert varV varT' (stateVarType s) })
		
		return $ Just (TVar kValue $ UVar varT')


lookupVtoT :: Var -> CSlurpM (Maybe Var)
lookupVtoT varV
 = do 	varType		<- gets stateVarType
	return		$ Map.lookup varV varType
	

getVtoT :: Var -> CSlurpM Var
getVtoT	varV
 = do 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return varT
	 Nothing	-> panic stage 
	 		$ "getVtoT: no type var bound for value var '" % varV % "'.\n"
			% "  bind = " % (show $ varId varV)	% "\n"
			% "  info = " % (show $ varInfo varV)	% "\n"

lbindVtoT :: Var -> CSlurpM Type
lbindVtoT varV
 = do
 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return $ TVar kValue $ UVar varT
	 Nothing	
	  -> do	Just v	<- bindVtoT varV
	  	return	v
	 
	
-- | Add a DataDef to the CSlurp state.
--   This function gets called when any top level TData nodes are encountered.
addDataDef :: (Top Annot2) -> CSlurpM ()
addDataDef ddef@(PData _ def)
 	= modify (\s -> s 
		{ stateDataDefs = Map.insert (dataDefName def) def (stateDataDefs s) })

addDef :: Var -> Type -> CSlurpM ()
addDef v t
 	= modify (\s -> s 
		{ stateSlurpDefs = Map.insert v t (stateSlurpDefs s)})

-----
addError :: Error -> CSlurpM ()
addError err
 = modify (\s -> s { stateErrors = err : stateErrors s })


-----
wantTypeV :: Var -> CSlurpM ()
wantTypeV v
	= modify (\s -> s { stateTypesRequest = Set.insert v (stateTypesRequest s) })
 
wantTypeVs :: [Var] -> CSlurpM ()
wantTypeVs vs
	= modify (\s -> s { stateTypesRequest = Set.union (Set.fromList vs) (stateTypesRequest s) })
 

-- Wrappers ---------------------------------------------------------------------------------------
-- value
newVarV		= newVarN  NameValue	
newVarVS	= newVarNS NameValue

-- data
newTVarD	= newVarN  NameType	   >>= \v -> return $ TVar kValue $ UVar v
newTVarDS s	= newVarNS NameType	s  >>= \v -> return $ TVar kValue $ UVar v

-- region
newTVarR	= newVarN  NameRegion	   >>= \v -> return $ TVar kRegion $ UVar v
newTVarRS s	= newVarNS NameRegion	s  >>= \v -> return $ TVar kRegion $ UVar v

-- effect
newTVarE	= newVarN  NameEffect	   >>= \v -> return $ TVar kEffect $ UVar v
newTVarES s	= newVarNS NameEffect	s  >>= \v -> return $ TVar kEffect $ UVar v

-- closure
newTVarC	= newVarN  NameClosure	   >>= \v -> return $ TVar kClosure $ UVar v
newTVarCS s	= newVarNS NameClosure	s  >>= \v -> return $ TVar kClosure $ UVar v

