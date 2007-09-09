
module Sea.Flatten
(
	flattenTree
)

where

import Util
import Shared.VarUtil
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Sea.Exp
import Sea.Pretty
import Sea.Plate.Trans

seaFlatten	= "xEF"

-----
type FlatM	= State Var.VarBind


flattenTree :: Tree () -> Tree ()
flattenTree tree	
 = 	evalState (flattenTreeM tree) 
 		$ Var.XBind seaFlatten 0


flattenTreeM :: Tree ()	-> FlatM (Tree ())
flattenTreeM	tree
 	= mapM (transformSSM flattenSS) tree
	
	
flattenSS ss
 = do
 	sss'	<- mapM flattenS ss
	return	$ concat sss'
	

-----
flattenS s
 = case s of
 	SMatch aa
	 -> do	
		-- create a label for the end of the match
		vMatchEnd	<- newVarNS NameLabel "_match_end"

	 	-- create a new label for each alt
	 	vsStart		<- mapM (\i -> newVarNS NameLabel ("_a" ++ show i)) 		[0.. length aa - 1]
 		vsExp		<- mapM (\i -> newVarNS NameLabel ("_a" ++ show i ++ "_exp"))	[0.. length aa - 1]

		let Just vsStartT
				= takeTail vsStart
 
	 	sss'		<- mapM (flattenA vMatchEnd) 
				$ zip5	[0..] 
					aa 
					vsStart 
					vsExp 
					(map (\v -> Just v) vsStartT ++ [Nothing])

	 	return	$  concat sss'
			++ [ SLabel vMatchEnd]
				
	_ ->	return [s]
	

	
-----
flattenA vMatchEnd (ixAlt, a, vStart, vExp, (mvStartNextAlt :: Maybe Var))
 = case a of
 	AAlt gs ss
	 -> do	gss	<- mapM (flattenG ixAlt mvStartNextAlt) $ zip [0..] gs
	 
	 	return	$  [ SComment ("a" ++ show ixAlt) ]
			++ [ SLabel vStart]
			++ concat gss
			++ [ SLabel vExp ] 
			++ ss
			++ [ SGoto  vMatchEnd ]
			++ [ SBlank ]

	ADefault ss
	 ->	return	$  [ SComment "alt default" ]
			++ [ SLabel vStart ]
	 		++ ss	 	
			++ [ SBlank ]


flattenG ixAlt mvStartNextAlt (ixGuard, g)
 = case g of
 	GCase ss x1 x2
	 -> do	let lName	= "_a" ++ show ixAlt ++ "g" ++ show ixGuard
	 
	 	vGuardStart	<- newVarNS NameLabel (lName)
	 	vGuardAgain	<- newVarNS NameLabel (lName ++ "_again")
		
		let aNext	= case mvStartNextAlt of
					Just label	-> [ADefault [SGoto label]]
					_		-> [ACaseDeath]

		let aFollow	= case (x1, x2) of
					(XTag x, XCon{})-> [ACaseSusp x vGuardAgain]
					_		-> []

		
		return	$  [ SLabel vGuardStart]
			++ ss
			++ [ SBlank ]
			++ [ SLabel vGuardAgain]
			++ [ SSwitch x1 
				(  [ ASwitch x2 [] ]
				++ aFollow
				++ aNext) ]
			++ [ SBlank ]
		
		




