
-- | Handles parsing of pragmas in source language
--   BUGS: pragma parse errors are ugly

module Source.Pragma
	( Pragma (..)
	, slurpPragmaTree)
where

import qualified Shared.Var	as Var
import Source.Exp
import Shared.Base
import Shared.Error
import Shared.Literal
import Util

stage	= "Source.Pragma"

data Pragma
	= PragmaCCInclude	String

slurpPragmaTree tree
	= concat $ map slurpPragma tree

slurpPragma pp@(PPragma (XVar sp v : _))
	| Var.name v == "cc_includes"
	= slurp_ccIncludes pp

	| otherwise
	= warning stage 
		("unknown pragma " % v % "\n")
		[]

slurpPragma _	= []
	
	
-- cc_includes
-- 	Adds an include statement directly into the emitted C program. 
--	Good for getting the correct function prototypes in ffi code.
slurp_ccIncludes (PPragma [XVar sp v, XList _ xStrs])
 = let	Just strs	= sequence $ map slurpConstStr xStrs
   in	map PragmaCCInclude strs
   

slurpConstStr :: Exp -> Maybe String
slurpConstStr xx
 = case xx of
	(XConst _ (CConst (LString str)))	-> Just str
	_					-> Nothing

