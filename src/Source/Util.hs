module Source.Util
	( flattenApps
	, unflattenApps
	, sourcePosX )

where

-----
import Util

-----
import Shared.Error 		(panic)
import qualified Shared.Var	as Var
import Shared.Var		((=~=))
import Shared.Base
import Source.Exp

-----
stage	= "Source.Util"

-----
-- flattenApps
--	| Converts some function applications into a list of expressions.
--
-- eg	   flattenApps (XApp (XApp (XApp x1 x2) x3) x4)
--    =>   [x1, x2, x3, x4]
--
flattenApps ::	Exp	-> [Exp]
flattenApps xx
 = case xx of
 	XApp sp x1 x2	-> flattenApps x1 ++ [x2]
	_		-> [xx]


-----
-- unflattenApps
--	| Converts a list of expressions into function applications.
--	
-- eg	   unflattenApps [x1, x2, x3, x4]
--	=> XApp (XApp (XApp x1 x2) x3) x4
--
unflattenApps :: SourcePos -> [Exp] -> Exp
unflattenApps sp (x:xs)
 = unflattenApps' sp x xs
 
unflattenApps' sp x xx
 = case xx of
 	[]	-> x
	xs	
	 -> let	Just xsL	= takeLast xs
	    in	XApp sp (unflattenApps' sp x (init xs)) xsL
	
-----
-- 
sourcePosX :: Exp -> SourcePos
sourcePosX xx 
 = case xx of
 	XNil				-> panic stage "sourcePosX: no source pos in XNil"
	XAnnot		aa x		-> sourcePosX x

	XUnit		sp		-> sp
	XVoid		sp		-> sp
	XConst		sp c		-> sp
	XVar 		sp v		-> sp
	XProj 		sp x j		-> sp
	XProjT		sp t j		-> sp
	XLambda	 	sp v x		-> sp
	XApp		sp x1 x2	-> sp
	XCase		sp x aa		-> sp
	XLet		sp ss x		-> sp
	XDo		sp ss		-> sp
	XIfThenElse	sp x1 x2 x3	-> sp
	XAppE		sp x1 x2 eff	-> sp
	XCaseE		sp x1 aa eff	-> sp
	XAt		sp v x		-> sp
	XObjVar		sp v		-> sp
	XObjField	sp v		-> sp
	XObjFieldR	sp v		-> sp
	XOp		sp v		-> sp
	XDefix		sp x		-> sp
	XDefixApps	sp x		-> sp
	XAppSusp	sp x1 x2	-> sp
	XLambdaPats	sp xx x		-> sp
	XLambdaProj	sp j xx		-> sp
	XLambdaCase	sp aa		-> sp
	XMatch		sp aa		-> sp
	XTry		sp x aa mx	-> sp
	XThrow		sp x		-> sp
	XWhile		sp x1 x		-> sp
	XWhen		sp x1 x2	-> sp
	XUnless		sp x1 x2	-> sp
	XBreak		sp		-> sp
	XListRange	sp b x mx	-> sp
	XListComp	sp x lc		-> sp
	XCon		sp v xx		-> sp
	XTuple		sp xx		-> sp
	XCons		sp x1 x2	-> sp
	XList 		sp xx		-> sp

--	_ -> panic stage
--		$ "sourcePosX: no match for " % show xx % "\n"	
