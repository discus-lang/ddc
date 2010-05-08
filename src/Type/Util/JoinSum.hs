
-- Joining the manifest effect and closure portions of a type.
--	Used when we choose between two functions of differing effects
--	eg: if .. then putStr else id
--
-- The value and region portions of the types must be the same,
--	but we can sum the effect and closure portions.
--	
-- Note that because we only strengthen constraints who's var does
--	not appear in a contra-variant branch, we don't end up
--	trying to join two types like:
--
-- t1:	(a -(!e1)>   b) -> c	:- !e1 :> !EFF2
-- t2:	(a -(!EFF1)  b) -> c	
--
-- To join the contra-variant effects we would have to change the constraint:
--
-- t1 `joinMax` t2
--	= (a -(!e1)> b) -> c	:- !e1 :> !EFF1 \/ !EFF2
--	
-- That would mean we'd have to extend the !e1 constraint in the 
--	environment, which is a hassle.
--
module Type.Util.JoinSum
	(joinSumTs)
where
import Type.Exp
import Type.Builtin
import Type.Util.Environment
import Type.Util.Bits
import Type.Util.Kind
import Control.Monad
import Util.Pretty
import DDC.Main.Error

stage	= "Type.Util.Join"

-- Join all these types.
--	The value and region portions must be the same.
--	The effect and closure portions are summed.
joinSumTs :: Env -> [Type] -> Maybe Type
joinSumTs env tt@(t:ts)
	= foldM (joinTT env) t ts


joinTT :: Env -> Type -> Type -> Maybe Type

-- Flip the args around to put the easy-to-unify argument first.
--	this means that the code for joinTT_work can get by with less cases.
joinTT env t1 t2
	| TVar{}		<- t2
	= joinTT_work env t2 t1

	| TVarMore{}		<- t2
	= joinTT_work env t2 t1

	| otherwise
	= joinTT_work env t1 t2

joinTT_work env t1 t2
	| Just (t11, t12, e1, c1)	<- takeTFun t1
	, Just (t21, t22, e2, c2)	<- takeTFun t2
	, t11 == t21
	, Just tY		<- joinTT env t12 t22
	, Just eff		<- joinTT env e1 e2
	, Just clo		<- joinTT env c1 c2
	= Just $ makeTFun t11 tY eff clo

	| TApp tX1 tY1		<- t1
	, TApp tX2 tY2		<- t2
	, Just tX		<- joinTT env tX1 tX2
	, Just tY		<- joinTT env tY1 tY2
	= Just $ TApp tX tY

	| TVar k1 v1		<- t1
	, TVar k2 v2		<- t2
	, v1 == v2	
	= Just $ TVar k1 v1

	| TVarMore k1 v1 b1	<- t1
	, TVarMore k2 v2 b2	<- t2
	, v1 == v2
	= Just $ TVarMore k1 v1 b1

	| TCon c1		<- t1
	, TCon c2		<- t2
	, c1 == c2	
	= Just $ TCon c1

	| TVar k1 v1		<- t1
	, Just k2		<- kindOfType t2
	, k1 == k2
	, k1 == kEffect || k1 == kClosure
	= Just $ makeTSum k1 [t1, t2]

	| TVarMore k1 v1 b1 <- t1
	, Just k2		<- kindOfType t2
	, k1 == k2
	, k1 == kEffect || k1 == kClosure
	= Just $ makeTSum k1 [t1, t2]

	| TSum k1 ts1		<- t1
	, Just k2 		<- kindOfType t2
	, k1 == k2
	, k1 == kEffect || k1 == kClosure
	= Just $ makeTSum k1 (t2 : ts1)

	| TSum k2 ts2		<- t2
	, Just k1		<- kindOfType t1
	, k1 == k2
	, k2 == kEffect || k2 == kClosure
	= Just $ makeTSum k2 (t2 : ts2)
	
	| otherwise
	= panic stage 
		(	"joinTT: cannot join\n"
		%> 	"t1:  " % t1 	% "\n"
		%> 	"t2:  " % t2)
		
	