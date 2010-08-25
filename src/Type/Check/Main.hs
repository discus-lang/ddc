
module Type.Check.Main
	(checkMain)
where
import Type.Extract
import Type.Error
import Shared.VarPrim
import Util
import DDC.Var
import DDC.Type
import DDC.Solve.State
import qualified Data.Map	as Map


-- | If the graph contains a function called 'main' then check 
--	that is has the appropriate type, () -> ()
checkMain :: SquidM ()
checkMain
 = do	sigmaTable	<- getsRef stateSigmaTable
 
	-- try and find an entry for the main variable in the sigma table
	--	this will tell us if we've seen the binding for it or not
 	let mMain	= find (\(v, t) -> isMainVar v)
			$ Map.toList sigmaTable
 
 	case mMain of
	 Nothing		-> return ()
	 Just (vMain, vMainT)	
	  -> do	Just tMain	<- extractType True vMainT
	  	checkMain' vMainT tMain tMain
	  	
-- | The main function is passed () by the runtime system at startup, 
--	so it must accept this.
checkMain' vMainT tMain tt
 = case tt of
	TForall b k t		-> checkMain' vMainT tMain t
 	TConstrain t crs	-> checkMain' vMainT tMain t

	TApp{}
	 | Just (TVar kV _, _, eff, clo)	<- takeTFun tt
	 , kV == kValue		
	 -> return ()
	
	 | Just (t1, _, eff, clo)		<- takeTFun tt
	 , Just (v1, _, _)			<- takeTData t1
	 , v1 == primTUnit
	 -> return ()
		 
	_ -> addErrors [ErrorWrongMainType { eScheme = (vMainT, tMain) }]
			
isMainVar var
	= varName var == "main"
