
-- | Flatten out match expressions.
module Sea.Flatten
	( flattenTree )

where

import Util
import Shared.VarGen
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import Sea.Exp
import Sea.Pretty
import Sea.Plate.Trans

-----
type FlatM	= VarGenM

-- Tree --------------------------------------------------------------------------------------------
flattenTree 
	:: String		-- unique
	-> Tree () 
	-> Tree ()

flattenTree unique tree	
 = 	evalVarGen (flattenTreeM tree) ("d" ++ unique)

flattenTreeM :: Tree ()	-> FlatM (Tree ())
flattenTreeM	tree
 	= mapM (transformSSM flattenSS) tree
		
flattenSS ss
 = do
 	sss'	<- mapM flattenS ss
	return	$ concat sss'
	

-- Stmt --------------------------------------------------------------------------------------------
flattenS :: Stmt () -> FlatM [Stmt ()]
flattenS s
 = case s of
 	SMatch aa
	 -> do	
		-- create a label for the end of the match
		vMatchEnd	<- newVarN_named NameLabel "match_end"

	 	-- create a new label for each alt
	 	vsStart		<- mapM (\i -> newVarN_named NameLabel ("a" ++ show i))
						[0.. length aa - 1]
 		vsExp		<- mapM (\i -> newVarN_named NameLabel ("a" ++ show i ++ "_exp"))
						[0.. length aa - 1]

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
	

	
-- Alt ---------------------------------------------------------------------------------------------
flattenA vMatchEnd (ixAlt, a, vStart, vExp, (mvStartNextAlt :: Maybe Var))
 = case a of
 	AAlt gs ss
	 -> do	gss	<- mapM (flattenG ixAlt mvStartNextAlt) $ zip [0..] gs
	 
	 	return	$  [ SComment ("alt" ++ show ixAlt) ]
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


-- Guard -------------------------------------------------------------------------------------------
flattenG 
	ixAlt 		-- the index of the alternative that this guard is in
	mvStartNextAlt 	-- maybe a label to jump to to get the next alternative
	( ixGuard	-- index of this guard
	 , g)		-- the guard

 = case g of
 	GCase isLazy ss x1 x2
	 -> do	let lName	= "a" ++ show ixAlt ++ "g" ++ show ixGuard
	
	 	vGuardStart	<- newVarN_named NameLabel (lName)
	 	vGuardAgain	<- newVarN_named NameLabel (lName ++ "_again")
		
		-- decide where to go if this guard fails
		let aNext
			-- if we've got another alternative after this one then go there.
			| Just label	<- mvStartNextAlt
			= [ADefault [SGoto label]]

			-- otherwise die and emit a non-exhaustive case match error
			| otherwise
			= [ACaseDeath]
		
		-- decide whether we need to generate code to follow suspensions.
		let (ssAgain, ssFollow)
			-- if we're matching against a constructor tag 
			--	and the constructor is in a lazy region, then yes
			| XTag x	<- x1
			, XCon{}	<- x2
			, isLazy
			= ([SLabel vGuardAgain], [ACaseSusp x vGuardAgain])

			-- otherwise no
			| otherwise
			= ([], [])


		-- build the expression		
		return	$  [ SLabel vGuardStart]
			++ ss
			++ [ SBlank ]
			++ ssAgain
			++ [ SSwitch x1 
				(  [ ASwitch x2 [] ]
				++ ssFollow
				++ aNext) ]
			++ [ SBlank ]
		

