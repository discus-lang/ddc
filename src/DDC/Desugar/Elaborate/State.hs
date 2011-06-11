{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | State for the elaborator.
module DDC.Desugar.Elaborate.State
	( ElabM
	, ElabS(..)
	, stateInit
	, newVarN
	, getKind
	, solveConstraints
	, addConstraint
	, getClassKinds
	, addClassKinds)
where
import Control.Monad.State.Strict
import DDC.Desugar.Elaborate.Constraint
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import DDC.Type
import Data.Map			(Map)
import Data.Sequence		(Seq)
import qualified Data.Map	as Map
import qualified Data.Foldable	as Foldable


stage	= "DDC.Desugar.Elaborate.State"

type ElabM = State ElabS
data ElabS
	= ElabS 
	{ stateVarGen	:: VarId 
	, stateKinds	:: Map Var Kind
	, stateClasses	:: Map Var [Kind] }
	

-- | Initial state for the elaboratot.
stateInit :: String -> ElabS
stateInit unique
	= ElabS
	{ stateVarGen	= VarId unique 0
	, stateKinds	= Map.empty
	, stateClasses	= Map.empty }
	

-- | Create a fresh variable in a given `NameSpace`.
newVarN :: NameSpace -> ElabM Var
newVarN space
 = do	vid@(VarId p i)	<- gets stateVarGen
 
	let name	= charPrefixOfSpace space : p ++ show i
	let var		= (varWithName name) 
			{ varId 	= vid
			, varNameSpace 	= space }
	
	modify $ \s -> s { stateVarGen = VarId p (i + 1) }
	
	return var


-- | Get the kind of a variable
getKind :: Var -> ElabM Kind
getKind v
 = do	kindMap	<- gets stateKinds
 	case Map.lookup v kindMap of
	 Just k		-> return k
	 Nothing	-> panic stage
	 		$ "getKind: no kind for" %% v


-- | Solve these kind constraints and add the resulting kinds to the state.
--   TODO: This isn't finished.
solveConstraints :: Seq Constraint -> ElabM ()
solveConstraints constraints
 = do	Foldable.mapM_ addConstraint constraints
	return ()
 	

-- | Add a contraint to the state
addConstraint :: Constraint -> ElabM ()
addConstraint (Constraint _ v k)
 = do	s	<- get

 	case Map.lookup v (stateKinds s) of
	 Nothing	
	  -> do	let state'	= s { stateKinds = Map.insert v k (stateKinds s) }
	  	put state'
		return	()
		
	 Just k'
	  -> addConstraint_unify v k k'
	 
addConstraint_unify v k k'
	| k == k'
	= return ()
	
	| otherwise
	= panic stage
	$ "addConstraint_unify: can't unify kinds for" %% v %% parens k %% parens k'


-- | Get the kinds of a typeclass' arguments
getClassKinds :: Var -> ElabM [Kind]
getClassKinds v
 = do	classes	<- gets stateClasses
 	case Map.lookup v classes of
	 Just k		-> return k
	 Nothing	-> panic stage
	 		$ "getClassKinds: no arguments for typeclass " %% v

-- | Add kind of class' arguments to map
addClassKinds :: Seq (Var,[Kind]) -> ElabM ()
addClassKinds classes
 = do	Foldable.mapM_ addClassKind classes
	return ()

addClassKind :: (Var,[Kind]) -> ElabM ()
addClassKind (v,ks)
 = do	s <- get
	let s' = s { stateClasses = Map.insert v ks (stateClasses s) }
	put s'
