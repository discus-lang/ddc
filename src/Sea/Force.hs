
-- | For each of the switch statements in a program,
--	add the alternatives that force suspensions and follow indirections.
module Sea.Force
	(forceTree)
where
import Sea.Plate.Trans
import Util
import DDC.Sea.Exp
import DDC.Var
import Shared.VarUtil		(VarGenM, newVarN, varPos)
import qualified Shared.Unique	as Unique

-----
type ForceM	= VarGenM

-- | Add forcing code to all switch statements in this tree.
forceTree ::  Tree () -> Tree ()
forceTree tree
 	= evalState (mapM (transformSSM forceSS) tree)
	$ VarId Unique.seaForce 0


forceSS :: [Stmt ()] -> ForceM [Stmt ()]
forceSS	ss
 = do	sss'	<- mapM forceS ss
	return	$ concat sss'


-- | If this statement is a switch, then add forcing code to it.
--	Otherwise return it unharmed.
forceS :: Stmt () -> ForceM [Stmt ()]
forceS s
 = case s of
 	SSwitch x@(XTag (XVar var vt)) aa
	 -> do	label	<- newVarN NameLabel
		let gS	=  SLabel label

		-- Add the macro to force suspensions and follow indirs.
		let aaF	=  aa
			++ [ ACaseSusp (XVar var vt) label
			   , ACaseIndir (XVar var vt) label ]

		-- If there is no default alternative then add the default one to
		--	throw an error.
		let aaD	= if or $ map (=@= ADefault{}) aaF
				then aaF
				else aaF ++ [ACaseDeath (varPos var)]

		let s'	= SSwitch x aaD

	 	return $  [gS, s']

	_ -> return [s]
