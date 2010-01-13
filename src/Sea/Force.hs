
module Sea.Force
	(forceTree)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import Shared.Base		(SourcePos(..))
import qualified Shared.Var	as Var
import Shared.Var		(VarBind(..), NameSpace(..))
import Shared.VarUtil		(VarGenM, newVarN, varPos)

import qualified Shared.Unique	as Unique

import Sea.Exp
import Sea.Pretty
import Sea.Plate.Trans


-----
type ForceM	= VarGenM

-----
forceTree :: 	Tree () -> Tree ()
forceTree	tree
 	= evalState (mapM (transformSSM forceSS) tree)
	$ Var.XBind Unique.seaForce 0
	

forceSS ::	[Stmt ()] -> ForceM [Stmt ()]
forceSS	ss
 = do
 	sss'	<- mapM forceS ss
	return	$ concat sss'
 

forceS ::	Stmt () -> ForceM [Stmt ()]
forceS		s
 = case s of
 	SSwitch x@(XTag (XVar var vt)) aa
	 -> do
		label	<- newVarN NameLabel
		
		let gS	=  SLabel label

		-- Add the macro to force suspensions and follow indirs.
		--
		let aaF	=  aa
			++ [ ACaseSusp (XVar var vt) label ]
		
		-- If there is no default alternative then add the default one to
		--	throw an error.
		--
		let aaD	= if or $ map (=@= ADefault{}) aaF
				then aaF
				else aaF ++ [ACaseDeath (varPos var)]
		
		let s'	= SSwitch x aaD
				
	 	return $  [gS, s']
	 
	_ -> return [s]

