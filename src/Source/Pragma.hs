
module Source.Pragma
	( slurpLinkObjs
	, slurpInclude)

where

import qualified Shared.Var	as Var
import Source.Exp
import Shared.Base
import Shared.Literal

slurpLinkObjs tree
	= [str 	 | XConst  sp (CConst (LString str)) 	<- concat
	  [xStrs | PPragma [XVar sp v, XList _ xStrs] 	<- tree
		 , Var.name v == "LinkObjs"]]

slurpInclude tree
	= [str	 | XConst  sp (CConst (LString str))	<- concat
	  [xStrs | PPragma [XVar sp v, XList _ xStrs]	<- tree
	  	 , Var.name v == "Include"]]
