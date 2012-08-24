{-# OPTIONS -Werror #-}

module DDCI.Core.Command.TransInteract
        ( cmdTransInteract
	, cmdTransInteractLoop)
where
import DDCI.Core.Command.Trans

import DDCI.Core.Command.Check
import DDCI.Core.Output
import DDCI.Core.State
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
 | Bundle fragment modules _ _ _ _	<- stateBundle state
 =   cmdParseCheckExp state fragment modules True source str 
 >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = do   return state

        -- Expression is well-typed.
        goStore (Just (x, t1, eff1, clo1))
	 | Bundle fragment modules zero simp rules _ <- stateBundle state
         = do   let hist   = TransHistory
			     { historyExp   = (x, t1, eff1, clo1)
			     , historySteps = [] }
		let bundle = Bundle fragment modules zero simp rules (Just hist)
		return state { stateTransInteract = True
			     , stateBundle = bundle }


cmdTransInteractLoop :: State -> String -> IO State
cmdTransInteractLoop state str
 | Bundle fragment modules zeroB simp rules (Just hist) <- stateBundle state
 , Fragment profile _ _ _ _ _ mkNamK mkNamT zero	<- fragment
 = case str of
    ":back" -> do
	let steps' = case historySteps hist of
		      []     -> []
		      (_:ss) -> ss

	putStrLn "Going back: "
	let (x,_,_,_) = case steps' of
		[]    -> historyExp hist
		(x':_) -> case historyExp hist of
			 (_,t,e,c) -> (x',t,e,c)
	outDocLn state $ ppr x

	let hist'      = hist { historySteps = steps' }
	return state { stateBundle = Bundle fragment modules zeroB simp rules (Just hist') }

    ":done" -> do
	return state { stateTransInteract = False }

    _	    -> do
 	let tr = parseSimplifier 
		    mkNamK mkNamT 
		    (Map.assocs rules) 
		    -- Collect all definitions from modules
		    (I.lookupTemplateFromModules $ Map.elems modules)
		    -- Module-specific templates
		    (map (\(n,m) -> (n, I.lookupTemplateFromModule m)) $ Map.assocs modules)
		    str

	let x = case historySteps hist of
		[]    -> historyExp hist
		(x':_) -> case historyExp hist of
			 (_,t,e,c) -> (x',t,e,c)

	case tr of
	    Just tr' -> do
                let kenv    = modulesExportKinds modules (profilePrimKinds profile)
                let tenv    = modulesExportTypes modules (profilePrimTypes profile)

		x' <- applyTransAndCheck state profile kenv tenv
			    zero tr' x

		case x' of
		    Nothing -> return state
		    Just x'' -> do
			outDocLn state $ ppr x''
			let steps' = x'' : historySteps hist
			let hist'  = hist { historySteps = steps' }
			return state { stateBundle = Bundle fragment modules zero tr' rules (Just hist') }

	    Nothing -> do
		putStrLn "Error parsing simplifier"
		return state

 | otherwise = error "No transformation history!"
