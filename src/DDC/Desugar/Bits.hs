{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Bits and pieces for working with desugared expresions.
module DDC.Desugar.Bits
	( -- * Predicates
	  isXVar
	
	  -- * Compounds
	, takeAnnotX
	, getAnnotW
	, addLambdas 
	, unflattenApps

	  -- * Binding vars
	, bindingVarsOfStmt
	, bindingVarsOfPat
	, bindingVarsOfGuard
	, bindingVarOfStmt

	  -- * Substitution
	, substituteVV
		
	  -- * Collection
	, collectClosureProjTags)

where
import DDC.Desugar.Exp
import DDC.Type
import DDC.Var
import Desugar.Plate.Trans
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import Data.Set				(Set)
import Data.Map				(Map)
import Util.Data.List
import Control.Monad.State.Strict


-- Predicates -------------------------------------------------------------------------------------
-- | Check if some exp is an @XVar@
isXVar :: Exp a -> Bool
isXVar xx
 = case xx of
 	XVar{}	-> True
	_	-> False

-- Compounds --------------------------------------------------------------------------------------
-- | Get the annotation from this expression.
takeAnnotX :: Exp a -> Maybe a
takeAnnotX xx
 = case xx of
 	XNil				-> Nothing
	XVoid 		n		-> Just n
	XLit		n _		-> Just n
	XVar 		n _		-> Just n
	XProj 		n _ _		-> Just n
	XLambda 	n _ _		-> Just n
	XApp 		n _ _		-> Just n
	XMatch 		n _ _		-> Just n
	XDo		n _		-> Just n
	XIfThenElse	n _ _ _		-> Just n
	XLambdaTEC	n _ _ _ _ _	-> Just n
	XProjTagged	n _ _ _ _	-> Just n
	XProjTaggedT	n _ _ _ 	-> Just n
	XProjT		n _ _ 		-> Just n
	XVarInst	n _		-> Just n
	
	
-- | Get the annotation from this pattern.
getAnnotW :: Pat a -> a
getAnnotW ww
 = case ww of
	WConLabel	n _ _		-> n
	WLit		n _		-> n
	WVar		n _		-> n
	WAt		n _ _		-> n
	WConLabelP	n _ _		-> n

	
-- | Add some lambdas to the front of an expression.
addLambdas 
	:: a	 	-- ^ Annotation to use on new nodes.
	-> [Var]	-- ^ Vars to bind.
	-> Exp a 	-- ^ Expression to use as the body.
	-> Exp a

addLambdas _ [] x	= x
addLambdas sp (v:vs) x	= XLambda sp v (addLambdas sp vs x)


-- | Make some value applications.
unflattenApps :: a -> Exp a -> [Exp a] -> Exp a
unflattenApps  a x xs
 = unflattenApps' a x xs
 
unflattenApps' a x xx
 = case xx of
 	[]	-> x
	xs	
	 -> let	Just xsL	= takeLast xs
	    in	XApp a (unflattenApps' a x (init xs)) xsL

	
-- Binding variables ------------------------------------------------------------------------------
-- | Determine the vars being bound by a statement lhs
bindingVarsOfStmt :: Stmt a -> Set Var
bindingVarsOfStmt ss
 = case ss of
	SBind 		_ Nothing _	-> Set.empty
	SBind 		_ (Just v) _	-> Set.singleton v
	SBindMonadic 	_ w _		-> bindingVarsOfPat w
	SBindPat	_ w _		-> bindingVarsOfPat w
	SSig		_ vs _		-> Set.fromList vs


-- | Determine the vars being bound by a pattern lhs
bindingVarsOfPat :: Pat a -> Set Var
bindingVarsOfPat ww
 = case ww of
	WConLabel 	_ _ lvs		-> Set.fromList $ map snd lvs
	WLit		_ _		-> Set.empty
	WVar		_ v		-> Set.singleton v
	WAt		_ v w		-> Set.unions [Set.singleton v, bindingVarsOfPat w]
	WConLabelP	_ _ lws		-> Set.unions $ map bindingVarsOfPat $ map snd lws


-- | Determine the vars being bound by a guard.
bindingVarsOfGuard :: Guard a -> Set Var
bindingVarsOfGuard gg
 = case gg of
	GCase 		_ w		-> bindingVarsOfPat w
	GExp		_ w _		-> bindingVarsOfPat w


-- | Take the binding var from a statement, if any.
bindingVarOfStmt :: Stmt a -> Maybe Var
bindingVarOfStmt ss
 = case ss of
	SBind _ mv _	-> mv
	_		-> Nothing


-- Substitution ------------------------------------------------------------------------------------
-- | Substitute vars for vars in an expression
substituteVV :: Map Var Var -> Exp a -> Exp a
substituteVV sub xx
 = let	transTable
	 =	(transTableId (\x -> return x))
		{ transV
		    = \v -> case Map.lookup v sub of
				Just v'	-> return v'
				_	-> return v }
   in	evalState (transZM transTable xx) ()
				 

-- Collection -------------------------------------------------------------------------------------
-- | Collect all the closure tag vars from the
--	TProjTagged constructors in this expression
collectClosureProjTags :: Exp a -> [Closure]
collectClosureProjTags xxx
 = let	takeV xx
	 = case xx of
		XProjTagged _ _ tC _ _
	 	 -> do	modify $ \s -> tC : s
			return xx
			
		XProjTaggedT _ _ tC _
		 -> do	modify $ \s -> tC : s
			return xx
			
		_ -> return xx
		
	table	= (transTableId (\x -> return x))
			{ transX_enter = takeV  }

   in	execState (transZM table xxx) []
