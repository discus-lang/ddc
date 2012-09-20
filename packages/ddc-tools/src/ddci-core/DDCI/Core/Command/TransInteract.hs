{-# OPTIONS -Werror #-}

module DDCI.Core.Command.TransInteract
        ( cmdTransInteract
	, cmdTransInteractLoop)
where
import DDCI.Core.Command.Trans
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Main.Command.Check
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Simplifier
import DDC.Base.Pretty
import qualified Data.Map               as Map
import qualified DDC.Core.Transform.Inline.Templates	as I

import DDC.Core.Module



-- TransInteract --------------------------------------------------------------
-- | Apply the current transform to an expression.
cmdTransInteract :: State -> Source -> String -> IO State
cmdTransInteract state source str
 | Bundle fragment modules _ _ rules		    <- stateBundle state
 , Fragment profile _ _ _ _ _ mkNamT mkNamX zero    <- fragment
 =   cmdParseCheckExp fragment modules True source str 
 >>= goStore mkNamT mkNamX zero profile modules rules
 where
        -- Expression had a parse or type error.
        goStore _ _ _ _ _ _ Nothing
         = do   return state

        -- Expression is well-typed.
        goStore mkNamT mkNamX zero profile modules rules
		(Just (x, t1, eff1, clo1))
         = do   let hist   = TransHistory
			     { historyExp	    = (x, t1, eff1, clo1)
			     , historySteps	    = []
			     , historyMakeNamifierT = mkNamT
			     , historyMakeNamifierX = mkNamX
			     , historyNameZero	    = zero
			     , historyProfile	    = profile
			     , historyModules	    = modules
			     , historyRewriteRules  = rules }
		return state { stateTransInteract = Just hist }


cmdTransInteractLoop :: State -> String -> IO State
cmdTransInteractLoop state str
 | Just hist						    <- stateTransInteract state
 , TransHistory (x,t,e,c) steps mkNamT mkNamX zero profile modules rules <- hist
 = case str of
    ":back" -> do
	let steps' = case steps of
		      []     -> []
		      (_:ss) -> ss

	putStrLn "Going back: "
	let x' = case steps' of
		   []    -> x
		   ((xz,_):_) -> xz
	outDocLn state $ ppr x'

	let hist'      = TransHistory (x,t,e,c) steps' mkNamT mkNamX zero profile modules rules
	return state { stateTransInteract = Just hist' }

    ":done" -> do
	let simps = reverse $ map (indent 4 . ppr . snd) steps
	outStrLn state "* TRANSFORM SEQUENCE:"
	mapM_ (outDocLn state) simps
	return state { stateTransInteract = Nothing }

    _	    -> do
 	let tr = parseSimplifier 
		    mkNamT mkNamX
		    (Map.assocs rules) 
		    -- Collect all definitions from modules
		    (I.lookupTemplateFromModules $ Map.elems modules)
		    -- Module-specific templates
		    (map (\(n,m) -> (n, I.lookupTemplateFromModule m)) $ Map.assocs modules)
		    str

	let x' = case steps of
		[]    -> x
		((xz,_):_) -> xz

	case tr of
	    Just tr' -> do
                let kenv    = modulesExportKinds modules (profilePrimKinds profile)
                let tenv    = modulesExportTypes modules (profilePrimTypes profile)

		x_trans <- applyTransAndCheck state profile kenv tenv
			    zero tr' (x', t, e, c)

		case x_trans of
		    Nothing -> return state
		    Just x_trans' -> do
			outDocLn state $ ppr x_trans'
			let steps' = (x_trans', tr') : steps
		        let hist'  = TransHistory (x,t,e,c) steps' mkNamT mkNamX zero profile modules rules
			return state { stateTransInteract = Just hist' }

	    Nothing -> do
		putStrLn "Error parsing simplifier"
		return state

 | otherwise = error "No transformation history!"
