module DDCI.Core.Command.TransInteract
        ( cmdTransInteract
	, cmdTransInteractLoop)
where
import DDCI.Core.Command.Trans
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Driver.Command.Check
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Core.Simplifier.Parser
import DDC.Core.Compounds
import DDC.Core.Check
import DDC.Core.Module
import DDC.Base.Pretty
import qualified Data.Map                       as Map
import qualified DDC.Core.Transform.Inline      as I


-- TransInteract --------------------------------------------------------------
-- | Apply the current transform to an expression.
cmdTransInteract :: State -> Source -> String -> IO State
cmdTransInteract state source str
 | Language bundle      <- stateLanguage state
 , fragment             <- bundleFragment   bundle
 , modules              <- bundleModules    bundle
 , zero                 <- bundleStateInit  bundle
 , profile              <- fragmentProfile  fragment
 , mkNamT               <- bundleMakeNamifierT bundle
 , mkNamX               <- bundleMakeNamifierX bundle
 , rules                <- bundleRewriteRules  bundle
 =   cmdParseCheckExp fragment modules True source str 
 >>= goStore mkNamT mkNamX zero profile modules rules
 where
        -- Expression had a parse or type error.
        goStore _ _ _ _ _ _ Nothing
         = do   return state

        -- Expression is well-typed.
        goStore mkNamT mkNamX zero profile modules rules (Just xx)
         = do   
                let Just annot  = takeAnnotOfExp xx
                let t1          = annotType annot
                let eff1        = annotEffect annot
                let clo1        = annotClosure annot

                let hist   = TransHistory
			     { historyExp	    = (xx, t1, eff1, clo1)
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
		    (SimplifierDetails
                        mkNamT mkNamX
        		(Map.assocs rules) 

                        -- Collect all definitions from modules
                        (I.lookupTemplateFromModules $ Map.elems modules)

                        -- Module-specific templates
                        (map (\(n,m) -> (n, I.lookupTemplateFromModules [m])) 
                                $ Map.assocs modules))
		    str

	let x' = case steps of
		[]    -> x
		((xz,_):_) -> xz

	case tr of
	    Just tr' -> do
                let kenv    = modulesExportKinds modules (profilePrimKinds profile)
                let tenv    = modulesExportTypes modules (profilePrimTypes profile)

		x_trans <- applyTransAndCheck state profile kenv tenv
			    zero tr' x'

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
