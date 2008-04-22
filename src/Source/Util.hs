module Source.Util
	( takeExportVar
	, takeStmtBoundVs
	, takePatBoundVs
	, flattenApps
	, unflattenApps
	, sourcePosX 
	, sourcePosW)

where

import Shared.Error 		(panic)
import qualified Shared.Var	as Var
import Shared.Var		((=~=))
import Shared.Base
import Source.Exp

import Util

-----
stage	= "Source.Util"

-- | take the var from an Eport
takeExportVar :: Export a -> Var
takeExportVar xx
 = case xx of
	EValue _ v	-> v
	EType _ v	-> v
	ERegion _ v	-> v
	EEffect _ v	-> v
	EClass _ v	-> v

-- | take the vars which are bound by this statement of this statement
takeStmtBoundVs :: Stmt a -> [Var]
takeStmtBoundVs s
 = case s of 
	SStmt 	  	sp e		-> []
	SBindFun 	sp v  es x	-> [v]
	SBindPat	sp pat x	-> takePatBoundVs pat
	SBindMonadic 	sp pat x	-> takePatBoundVs pat
	SSig      	sp v  t		-> [v]
	
-- | take the vars which are bound by this pattern
takePatBoundVs :: Pat a -> [Var]
takePatBoundVs w
 = case w of
 	WVar 		sp v		-> [v]
	WObjVar		sp v		-> [v]
	WConst		sp _		-> []
	WCon		sp v ws		-> v : catMap takePatBoundVs ws
	WConLabel 	sp v lws	-> v : catMap (takePatBoundVs . snd) lws
	WAt		sp v w		-> v : takePatBoundVs w
	WWildcard	sp		-> []
	WUnit		sp		-> []
	WTuple		sp ws		-> catMap takePatBoundVs ws
	WCons		sp w1 w2	-> takePatBoundVs w1 ++ takePatBoundVs w2
	WList		sp ws		-> catMap takePatBoundVs ws
		
-- | Convert some function applications into a list of expressions.
--
-- eg	   flattenApps (XApp (XApp (XApp x1 x2) x3) x4)
--    =>   [x1, x2, x3, x4]
--
flattenApps ::	Exp a	-> [Exp a]
flattenApps xx
 = case xx of
 	XApp sp x1 x2	-> flattenApps x1 ++ [x2]
	_		-> [xx]

-- | Convert a list of expressions into function applications.
--	
-- eg	   unflattenApps [x1, x2, x3, x4]
--	=> XApp (XApp (XApp x1 x2) x3) x4
--
unflattenApps :: a -> [Exp a] -> Exp a
unflattenApps sp (x:xs)
 = unflattenApps' sp x xs
 
unflattenApps' sp x xx
 = case xx of
 	[]	-> x
	xs	
	 -> let	Just xsL	= takeLast xs
	    in	XApp sp (unflattenApps' sp x (init xs)) xsL
	
	
-- | Slurp out the source position from this expression
sourcePosX :: Exp SourcePos -> SourcePos
sourcePosX xx 
 = case xx of
 	XNil				-> panic stage "sourcePosX: no source pos in XNil"
	XConst		sp c		-> sp
	XVar 		sp v		-> sp
	XProj 		sp x j		-> sp
	XProjT		sp t j		-> sp
	XApp		sp x1 x2	-> sp
	XCase		sp x aa		-> sp
	XLet		sp ss x		-> sp
	XDo		sp ss		-> sp
	XIfThenElse	sp x1 x2 x3	-> sp
	XObjField	sp v		-> sp
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
	XTuple		sp xx		-> sp
	XList 		sp xx		-> sp


-- | Slurp out the source position from some pattern
sourcePosW :: Pat SourcePos -> SourcePos
sourcePosW xx
 = case xx of
 	WVar 		sp v		-> sp
	WObjVar		sp v		-> sp
	WConst		sp c		-> sp
	WCon		sp v ws		-> sp
	WConLabel	sp v lws	-> sp
	WAt		sp v p		-> sp
	WWildcard	sp 		-> sp
	WUnit		sp		-> sp
	WTuple		sp ws		-> sp
	WCons		sp w1 w2	-> sp
	WList 		sp ws		-> sp

