
module Core.Bind
(
	bindTree
)

where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.Error

import Util
import Core.Plate.Walk
import Core.Plate.FreeVars
import Core.Exp
import Core.Util

type	BindM	= State ()

-----

stage	= "Core.Bind"
-----


bindTree 
	:: (Var -> Maybe [Class])
	-> Tree	
	-> Tree
	
bindTree lookupFs tree
	= map (bindP lookupFs Map.empty) tree


-----
bindP 	:: (Var -> Maybe [Class]) -> Map Var Type -> Top -> Top
bindP	lookupFs tt pp
 = case pp of
 	PBind v x	-> PBind v (bindX lookupFs tt x)
	_		-> pp
	

bindX 	:: (Var -> Maybe [Class]) -> Map Var Type -> Exp -> Exp	
bindX 	lookupFs tt xx
 = case xx of
 	XLAM  v t x	
	 -> let	tt'	= Map.insert v t tt
	    	x'	= bindX lookupFs tt' x
            in	XLAM v t x'
	    
	XTet vts x
	 -> let	x'	= bindX lookupFs tt x
	    in	XTet vts x'
	    
	XTau t x
	 -> let x'	= bindX lookupFs tt x
	    in	XTau t x'
	    
	XLam v t x eff clo
	 -> let	x'	= bindX lookupFs tt x
	    in	XLam v t x' eff clo
	    
	XDo ss
	 -> bindXDo lookupFs tt xx

	_ -> panic stage $ "bindX: no match for " % show xx


bindXDo :: (Var -> Maybe [Class]) -> Map Var Type -> Exp -> Exp	
bindXDo lookupFs tt xx@(XDo ss)
 = let
 	-- Regions bound above this exp.
 	rsHigh		= Set.fromList $ Map.keys tt
	
	-- Regions free in this exp.
	vsHere		= Set.unions
			$ map freeVarsS ss
	
	rsHere		=  Set.fromList
			$ [ v | v <- Set.toList vsHere
				, Var.nameSpace v == NameRegion]
	
	-- Work out the regions which are local to this block.
	rsLocal		= Set.toList $ rsHere `Set.difference` rsHigh
	
	-- Check for fetters on these regions.
{-	rsLocalFs	
		= map 	(\r -> 	let fs	= getFetters r lookupFs
				    r'	= TVar KRegion r
				in  (r', defaultRegionFs r' fs))
		$ rsLocal
-}

	-- Append local region definitions to the expression.
	--	Reverse the order of there regions here so they come out 
	--	in the same order as they're present in the block.
	--	The actual order doesn't matter.
	--
	xx'	= foldl (\x (TVar KRegion v, fs) 
				-> let vFs	= map (\(TClass v ts) -> v) fs
				   in  XLocal v vFs x)
			(XDo ss)
			[] -- (reverse rsLocalFs)
			
   in	xx'

getFetters r lookupFetters 
 = case lookupFetters r of
 	Nothing	-> [] -- panic stage $ "getFetters: no fetters for " % r % "\n"
	Just fs	-> fs


-----------------------
-- defaultRegionFs
-- | 	Default region fetters to Const Direct.	 	
--
defaultRegionFs
	:: Region
	-> [Class]
	-> [Class]

defaultRegionFs r fs
 = let
	-- default regions to const.
	gotMutable	= or $ map (\(TClass v _) -> Var.bind v == Var.FMutable) fs
	gotConst	= or $ map (\(TClass v _) -> Var.bind v == Var.FConst)	 fs

	defaultFsC
		= if not gotMutable && not gotConst
			then [TClass primConst [r]]
			else []

	-- default regions to direct
	gotLazy		= or $ map (\(TClass v _) -> Var.bind v == Var.FLazy)	fs
	gotDirect	= or $ map (\(TClass v _) -> Var.bind v == Var.FDirect)	fs
	
	defaultFsD	
		= if not gotLazy && not gotDirect
			then [TClass primDirect [r]]
			else []

  in	fs ++ defaultFsC ++ defaultFsD
   
    
