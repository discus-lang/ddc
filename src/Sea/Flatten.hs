
-- | Flatten out match expressions into sequences of statements and gotos.
module Sea.Flatten
	(flattenTree)
where
import Sea.Plate.Trans
import Shared.VarGen
import Util
import DDC.Sea.Exp
import DDC.Sea.Compounds
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Var
import DDC.Sea.Pretty		()

-----
type FlatM	= VarGenM

-- | Flatten all match expressions in this tree.
flattenTree
	:: String		-- uniqueID
	-> Tree () 		-- input tree
	-> Tree ()		-- output tree with flattened match expressions.

flattenTree unique tree
 = 	evalVarGen (flattenTreeM tree) ("d" ++ unique)

flattenTreeM :: Tree ()	-> FlatM (Tree ())
flattenTreeM	tree
 	= mapM (transformSSM flattenSS) tree

flattenSS ss
 = do	sss'	<- mapM flattenS ss
	return	$ concat sss'


-- | If this statement is a match then flatten it,
--	otherwise return it unharmed.
flattenS
	:: Stmt ()
	-> FlatM [Stmt ()]

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
				$ zip5	([0..] :: [Integer])
					aa
					vsStart
					vsExp
					(map (\v -> Just v) vsStartT ++ [Nothing])

	 	return	$  concat sss'
			++ [ SLabel vMatchEnd]

	_ ->	return [s]


-- | Flatten a match alternative
flattenA vMatchEnd (ixAlt, a, vStart, vExp, (mvStartNextAlt :: Maybe Var))
 = case a of
 	AAlt gs ss
	 -> do	gss	<- mapM (flattenG ixAlt mvStartNextAlt) $ zip ([0..] :: [Integer]) gs

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

-- | Flatten a guard
flattenG
	ixAlt 		-- the index of the alternative that this guard is in
	mvStartNextAlt 	-- maybe a label to jump to to get the next alternative
	( ixGuard	-- index of this guard
	 , g)		-- the guard

	| GCase _ isLazy ss x1 x2@(XLit (LLit litFmt)) 	<- g
	, LiteralFmt lit fmt			<- litFmt
	= do
		let lName	= "a" ++ show ixAlt ++ "g" ++ show ixGuard
		vGuardStart	<- newVarN_named NameLabel (lName)

		-- decide where to go if this guard fails
		let ssNext
			-- if we've got another alternative after this one then go there.
			| Just label	<- mvStartNextAlt
			= [SGoto label]

			-- otherwise die and emit a non-exhaustive case match error
			| otherwise
			= [SCaseFail]

		let stmts
			| LiteralFmt (LString _) Unboxed <- litFmt
			= ss
			++ [ SIf (XPrim (MOp OpNeq) [XPrim (MFun PFunStrCmp) [x1, x2], xInt 0])
				 ssNext]

			| otherwise
			= ss
			++ [ SIf (XPrim (MOp OpNeq) [x1, x2])
			 	 ssNext]

		return stmts

 	| GCase spos isLazy ss x1 x2			<- g
	= do
		let lName	= "a" ++ show ixAlt ++ "g" ++ show ixGuard

	 	vGuardStart	<- newVarN_named NameLabel (lName)
	 	vGuardAgain	<- newVarN_named NameLabel (lName ++ "_again")

		-- decide where to go if this guard fails
		let aNext
			-- if we've got another alternative after this one then go there.
			| Just label	<- mvStartNextAlt
			= [ADefault [SGoto label]]

			-- otherwise die and emit a non-exhaustive case match error
			| otherwise
			= [ACaseDeath spos]

		-- decide whether we need to generate code to follow suspensions.
		let (ssAgain, ssFollow)
			-- if we're matching against a constructor tag
			--	and the constructor is in a lazy region, then yes
			| XTag x		<- x1
			, XLit LDataTag{}	<- x2
			, isLazy
			= ([SLabel vGuardAgain], [ACaseSusp x vGuardAgain, ACaseIndir x vGuardAgain])

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
