

-- | Conversion of variables who's types are instances of some scheme.
module DDC.Desugar.ToCore.VarInst
	(toCoreVarInst)
where
import DDC.Desugar.ToCore.Base
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import Type.ToCore
import qualified DDC.Type			as T
import qualified DDC.Solve.InstanceInfo		as T
import qualified DDC.Core.Exp			as C
import qualified Core.Util			as C
import qualified Data.Map			as Map
import qualified Debug.Trace			as Debug
import Control.Monad.State

stage		= "DDC.Desugar.ToCore.VarInst"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain ss) x else x

-- Add type applications around the occurrence of some var who's type is an 
-- instantiated scheme.
toCoreVarInst :: Var -> Var -> CoreM C.Exp
toCoreVarInst v vT
 = do
	Just tScheme	<- lookupType v
	mapInst		<- gets coreMapInst

	let (btsForall, ksContextC, tShape)
		= T.stripForallContextT tScheme
	
	-- lookup how this var was instantiated
	let Just instInfo = Map.lookup vT mapInst

	-- check how this var was instantiated to work out if we
	--	need to pass type args.
	case instInfo of

	 -- use of a lambda bound variable.
	 -- 	only rank1 polymorphism => lambda bound vars have monotypes
	 T.InstanceLambda vUse _ _
	  -> do	let xResult	= C.XVar v tShape	
		trace 	( "varInst: TInstanceLambda\n"
	  		% "    vUse    = " % vUse	% "\n"
			% "    tScheme = " % tScheme	% "\n"
			% "    tShape  = " % tShape	% "\n"
			% "    xResult = " % xResult	% "\n")
			$ return ()
		
	  	return $ xResult
	 -- non-recursive use of a let bound variable 
	 -- 	pass in the type args corresponding to the instantiated foralls.
	 T.InstanceLet _ _ tsInst _
	  -> do	
		-- Convert the type arguments to core.
		let tsInstC	= map (T.flattenT . toCoreT) tsInst
			
		-- If the function being instantiated needs some context then there'll be a 
		--	separate witness for it... therefore we can safely erase contexts on
		--	type arguements for the instantiation.
		let tsInstCE	= map T.stripToBodyT tsInstC
			
		let tsInstC_packed	= map (T.crushT . T.packType) tsInstCE
			
		-- Work out what types belong to each quantified var in the type
		--	being instantiated.			
		let Just vsSub	= sequence $ map (T.takeVarOfBind . fst) btsForall

		let tsSub	= Map.fromList 
				$ zip vsSub tsInstC_packed

		-- If this function needs a witnesses we'll just make them up.
		--	Real witnesses will be threaded through in a later stage.
		let ksContextC'	= map (C.substituteT (flip Map.lookup tsSub)) ksContextC
		let tsContextC' = map (T.packType)
				$ map (\k -> let Just t = T.inventWitnessOfKind (T.crushK k) in t)
				$ ksContextC'

		let Just xResult = 
			C.buildApp (Left (C.XVar v tScheme) : map Right (tsInstC_packed ++ tsContextC'))

		trace ("varInst: "
			% vT 				% "\n"
			% "    tScheme         =\n" %> tScheme 		% "\n\n"
			% "    ksContext       = " % ksContextC		% "\n"
			% "    tsInstC         = " % tsInstC            % "\n"
			% "    tsInstCE        = " % tsInstCE		% "\n"
			% "    tsInstC_packed  = " % tsInstC_packed	% "\n"
			% "    tsSub           = " % tsSub 		% "\n"
			% "    tsContestC'     = " % tsContextC' 	% "\n"
			% "    xResult         = " % xResult		% "\n")
			$ return ()

		return	$ xResult

	 -- recursive use of a let-bound variable
	 -- 	pass the args on the type scheme back to ourselves.
	 T.InstanceLetRec _ _ (Just tSchemeT)
	  -> do
		let tSchemeC	= T.flattenT $ toCoreT tSchemeT
				
		let (bksReplay, ksContext, _)
				= T.stripForallContextT tSchemeC

		let tsReplay
			= map (\b -> case b of
					(T.BNil,        _) -> panic stage "got T.BNil"
					(T.BVar  v',   k) -> T.TVar k (T.UVar  v')
					(T.BMore v' t, k) -> T.TVar k (T.UMore v' t))
			$ bksReplay

		let tsContext	= map (\k -> let Just t = T.inventWitnessOfKind k in t)
				$ ksContext

		let Just xResult =
			C.buildApp (Left (C.XVar v tSchemeC) : map Right (tsReplay ++ tsContext))

		return $ xResult

	 T.InstanceLetRec _ _ Nothing
		-> panic stage "toCoreVarInst: InstanceLetRec has no scheme"
