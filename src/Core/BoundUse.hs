
-- | Examines how bound value variables are used, how deep they are and whether
--	they appear inside an unboxing. Used by the optimiser.
--
module Core.BoundUse
	( Use (..), useLevel
	, UseM
	, boundUseGlob
	, boundUseP
	, boundUseX)
where
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Var
import Control.Monad.State.Strict
import Util
import qualified Data.Map		as Map


-- How many (value) lambdas in from top level this use was
type Level	= Int

-- | Records how a bound variable was used.
-- 	TODO: also add usematch if all uses are case matches can also do unboxed case match
--
data Use
	-- | a regular use
	= Use		Level
	
	-- | a use directly inside an unbox
	--	we detect this because it's ok to duplicate bindings like
	--	v1 = box v2, so long as all uses of v1 are directly inside unboxings
	| UseUnbox	Level

	deriving (Eq, Show)


-- | Take the level from a Use.
useLevel :: Use -> Level
useLevel uu
 = case uu of
 	Use l		-> l
	UseUnbox l	-> l


instance Pretty Use PMode where
 ppr use	= ppr $ show use
	

-- Use State Monad --------------------------------------------------------------------------------
-- | A monad for collecting uses of variables
type UseM	= State (Map Var [Use])


-- | Add a use of a variable to the state
addUse :: Var -> Use -> UseM ()
addUse v use
 = do	s	<- get
	let s'	= Map.alter (\e -> case e of
				Nothing	-> Just [use]
				Just us	-> Just (use : us))
			v
			s
	put s'


---------------------------------------------------------------------------------------------------
-- | Examine var usage in this glob
boundUseGlob :: Glob -> UseM ()
boundUseGlob cgModule
	= mapBindsOfGlobM_(boundUseP 0) cgModule


-- | Examine var usage in this top level thing.
boundUseP :: Level -> Top -> UseM ()
boundUseP level pp
 = case pp of
 	PBind v x	-> boundUseX level x
	_		-> return ()

	
-- | Examine var usage in this expression.
boundUseX :: Level -> Exp -> UseM ()
boundUseX level xx
 = case xx of
 	XLAM bind k x		-> boundUseX level x
	XAPP x t		-> boundUseX level x
	XTau vts x		-> boundUseX level x
	
	XLam v t x eff clo	-> boundUseX (level + 1) x

	XApp x1 x2 eff		
	 -> do	boundUseX level x1
	 	boundUseX level x2
		
	XDo ss			-> mapM_ (boundUseS level) ss
	XMatch alts		-> mapM_ (boundUseA level) alts
	XLocal v vts x		-> boundUseX level x
	XLit{}			-> return ()

	-- a regular use of a bound variable
	XVar v t		
	 -> 	addUse v (Use level)

	-- a use of a var directly inside an unbox
	XPrim MUnbox [r, XVar v t]
	 -> 	addUse v (UseUnbox level)
	
	XPrim p xs		-> mapM_ (boundUseX level) xs
	XPrimType{}		-> return ()

	
-- | Examine var usage in this stmt.
boundUseS :: Level -> Stmt -> UseM ()
boundUseS level (SBind mV x)	
	= boundUseX level x


-- | Examine var usage in this alternative.
boundUseA :: Level -> Alt -> UseM ()
boundUseA level (AAlt gs x)
 = do	mapM_ (boundUseG level) gs
 	boundUseX level x

	
-- | Examine var usave in this guard.
boundUseG :: Level -> Guard -> UseM ()
boundUseG level (GExp p x)	
	= boundUseX level x

