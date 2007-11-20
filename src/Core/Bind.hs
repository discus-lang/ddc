
module Core.Bind
	( bindTree )

where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.VarGen
import Shared.Error

import Util
import Core.Plate.Walk
import Core.Plate.FreeVars
import Core.Exp
import Core.Util
import Core.Pretty

import Debug.Trace

type   BindM	= VarGenM

-----

stage	= "Core.Bind"
-----

-- | Introduce local region definitions.
bindTree 
	:: String		-- ^ unique prefix to use for fresh vars
	-> (Var -> Maybe [Class])
	-> Tree			-- ^ core tree
	-> Tree			
	
bindTree unique lookupFs tree
	= evalVarGen (mapM (bindP lookupFs Map.empty) tree) ("w" ++ unique)

-----
bindP 	:: (Var -> Maybe [Class]) -> Map Var Type -> Top -> BindM Top
bindP	lookupFs tt pp
 = case pp of
 	PBind v x	
	 -> do	x'	<- bindX lookupFs tt x
	 	return	$ PBind v x'
		
	_		-> return pp
	

bindX 	:: (Var -> Maybe [Class]) -> Map Var Type -> Exp -> BindM Exp	
bindX 	lookupFs tt xx
 = case xx of
 	XLAM  v t x	
	 -> do	let tt'	= Map.insert v t tt
	    	x'	<- bindX lookupFs tt' x
            	return	$ XLAM v t x'
	    
	XTet vts x
	 -> do  x'	<- bindX lookupFs tt x
	 	return	$ XTet vts x'
	    
	XTau t x
	 -> do	x'	<- bindX lookupFs tt x
	 	return	$ XTau t x'
	    
	XLam v t x eff clo
	 -> do	x'	<- bindX lookupFs tt x
	    	return	$ XLam v t x' eff clo
	    
	XDo ss
	 -> bindXDo lookupFs tt xx

	_ -> panic stage $ "bindX: no match for " % show xx


bindXDo :: (Var -> Maybe [Class]) 
	-> Map Var Type 
	-> Exp 
	-> BindM Exp	
	
bindXDo lookupFs tt xx@(XDo ss)
 = do
 	-- Regions bound above this exp.
 	let rsHigh	= Set.fromList $ Map.keys tt
	
	-- Work out the regions free in this expression.
	let vsHere	= Set.unions
			$ map freeVarsS ss

	let rsHere	=  Set.fromList
			$ [ v | v <- Set.toList vsHere
			      , Var.nameSpace v == NameRegion]
	
	-- Work out the regions which are local to this block.
	let rsLocal	= Set.toList $ rsHere `Set.difference` rsHigh
	
	-- Check for fetters on these regions.
	rsLocalFs	
		<- mapM 
			(\r 
			 -> do  let fs	= getFetters r lookupFs
				let r'	= TVar KRegion r
				fs'	<- makeWitnesses r' fs
				return 	(r', fs'))
		$ rsLocal

	-- Append local region definitions to the expression.
	--	Reverse the order of there regions here so they come out 
	--	in the same order as they're present in the block.
	--	The actual order doesn't matter.
	--
	let xx'	= foldl
			(\x (TVar KRegion v, fs) -> XLocal v fs x)
			(XDo ss)
			(reverse rsLocalFs)
			
   	return	xx'

getFetters r lookupFetters 
 = []
 {-
 = case lookupFetters r of
 	Nothing	-> [] -- panic stage $ "getFetters: no fetters for " % r % "\n"
	Just fs	-> fs
-}


-- Construct appropriate witnesses for this regions constraints
makeWitnesses
	:: Region
	-> [Class]
	-> BindM [(Var, Class)]

makeWitnesses r fs
 = do
	-- default regions to const.
	let gotMutable	= or $ map (\(TClass v _) -> Var.bind v == Var.FMutable) fs
	let gotConst	= or $ map (\(TClass v _) -> Var.bind v == Var.FConst)	 fs

	defaultFsC <-
	   if not gotMutable && not gotConst
		then do	v	<- newVarN NameClass
			return	[(v, TClass primConst [r])]
		else return []

	-- default regions to direct
	let gotLazy	= or $ map (\(TClass v _) -> Var.bind v == Var.FLazy)	fs
	let gotDirect	= or $ map (\(TClass v _) -> Var.bind v == Var.FDirect)	fs
	
	defaultFsD <-
	   if not gotLazy && not gotDirect
		then do	v	<- newVarN NameClass
			return	[(v, TClass primDirect [r])]
		
		else return []

  	return $ defaultFsC ++ defaultFsD
   
    
