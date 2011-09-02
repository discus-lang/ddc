{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | State monad for projection desugarer.
module DDC.Desugar.Projections.Base
	( Annot
	, ProjectM
	, ProjectS(..)
	, stateInit
	, addError
	, newVarN
	, freshenVar
	, freshenCrsEq)
where
import DDC.Source.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Source.Desugar			(Annot)
import Control.Monad.State.Strict
import qualified Data.Map		as Map

type	ProjectM	= State ProjectS


-- | State for projection desugarer.
data ProjectS
	= ProjectS
	{ stateVarGen	:: VarId
	, stateErrors	:: [Error] }


-- | Create a new state.
stateInit :: String -> ProjectS
stateInit unique
	= ProjectS
	{ stateVarGen	= VarId unique 0
	, stateErrors	= [] }


-- | Add an error to the state monad.
addError :: Error -> ProjectM ()
addError err
	= modify $ \s -> s { stateErrors = err : stateErrors s }


-- | Allocate a fresh variable in this namespace.
newVarN :: NameSpace -> ProjectM Var
newVarN	space
 = do
 	varBind		<- gets stateVarGen
	let varBind'	= incVarId varBind
	modify $ \s -> s { stateVarGen = varBind' }

	let var		= (varWithName $ (charPrefixOfSpace space : pprStrPlain varBind))
			{ varId	= varBind
			, varNameSpace	= space }
	return var


-- | Create a new variable with the same info as the one, but give it this module name
--	and a fresh unique id.
freshenVar :: ModuleId -> Var -> ProjectM Var
freshenVar mid v
 = do	vNew		<- newVarN (varNameSpace v)
	let vFresh
		= v 	{ varId		= varId vNew
			, varModuleId	= mid }

	return vFresh


-- | If this type has eq constraints then give them fresh binding variables.
freshenCrsEq :: ModuleId -> Type -> ProjectM Type
freshenCrsEq mid tt
 = case tt of
	TForall b k t
	 -> do	t'	<- freshenCrsEq mid t
		return	$ TForall b k t'

	TConstrain _ crs
	 -> do	let takeSub (TVar k (UVar v))
		    	= do	vFresh	<- freshenVar mid v
			 	return	$ Just (TVar k $ UVar v, TVar k $ UVar vFresh)

		    takeSub _	= return Nothing

		Just subs	<- liftM sequence
				$  mapM takeSub
				$  Map.keys $ crsEq crs

		let vsSub	= Map.fromList subs

		return	$ subTT_everywhere vsSub tt

	_ -> return tt
