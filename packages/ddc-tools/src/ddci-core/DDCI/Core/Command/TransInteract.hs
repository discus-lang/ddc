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
import DDC.Core.Simplifier.Parser2
import DDC.Core.Compounds
import DDC.Core.Check
import DDC.Core.Module
import DDC.Base.Pretty
import qualified Data.Map                       as Map


-- TransInteract --------------------------------------------------------------
-- | Apply the current transform to an expression.
cmdTransInteract :: State -> Source -> String -> IO State
cmdTransInteract state source str
 | Language bundle      <- stateLanguage state
 , fragment             <- bundleFragment   bundle
 , modules              <- bundleModules    bundle
 =   cmdParseCheckExp fragment modules True source str 
 >>= goStore bundle
 where
        -- Expression had a parse or type error.
        goStore _ Nothing
         = do   return state

        -- Expression is well-typed.
        goStore bundle (Just xx)
         = do   
                let Just annot  = takeAnnotOfExp xx
                let t1          = annotType annot
                let eff1        = annotEffect annot
                let clo1        = annotClosure annot

                let hist   = TransHistory
			     { historyExp	    = (xx, t1, eff1, clo1)
			     , historySteps	    = []
                             , historyBundle        = bundle }

		return state { stateTransInteract = Just hist }


cmdTransInteractLoop :: State -> String -> IO State
cmdTransInteractLoop state str
 | Just hist    <- stateTransInteract state
 , TransHistory (x,t,e,c) steps bundle <- hist
 , fragment     <- bundleFragment bundle
 , profile      <- fragmentProfile fragment
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

	let hist'      = TransHistory (x,t,e,c) steps' bundle
	return state { stateTransInteract = Just hist' }

    ":done" -> do
	let simps = reverse $ map (indent 4 . ppr . snd) steps
	outStrLn state "* TRANSFORM SEQUENCE:"
	mapM_ (outDocLn state) simps
	return state { stateTransInteract = Nothing }

    _	    -> do

 	let tr = parseSimplifier 
                    (fragmentReadName fragment)
		    (SimplifierDetails
                        (bundleMakeNamifierT bundle) 
                        (bundleMakeNamifierX bundle)
        		(Map.assocs $ bundleRewriteRules bundle) 
                        (Map.elems  $ bundleModules      bundle))
		    str

	let x' = case steps of
		[]    -> x
		((xz,_):_) -> xz

	case tr of
            Left _err -> do
                putStrLn "Error parsing simplifier"
                return state

	    Right tr' -> do
                let kenv    = modulesExportKinds
                                (bundleModules bundle) 
                                (profilePrimKinds profile)

                let tenv    = modulesExportTypes
                                (bundleModules bundle) 
                                (profilePrimTypes profile)

		x_trans <- applyTransAndCheck state profile kenv tenv
			    (bundleStateInit bundle) tr' x'

		case x_trans of
		    Nothing -> return state
		    Just x_trans' -> do
			outDocLn state $ ppr x_trans'
			let steps' = (x_trans', tr') : steps
		        let hist'  = TransHistory (x,t,e,c) steps' bundle
			return state { stateTransInteract = Just hist' }

 | otherwise = error "No transformation history!"
