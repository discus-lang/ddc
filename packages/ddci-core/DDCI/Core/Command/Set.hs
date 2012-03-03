
-- | Display and toggle active interpreter modes.
module DDCI.Core.Command.Set
        ( Mode(..)
        , cmdSet)
where
import DDCI.Core.State
import qualified DDCI.Core.Rewrite as R
import DDCI.Core.Transform
import DDCI.Core.Mode
import DDCI.Core.IO
import DDC.Base.Pretty
import Data.Char
import qualified Data.Set       as Set
import qualified Data.Map       as Map


cmdSet ::  State -> String -> IO State

-- Display the active modes.
cmdSet state []
 = do   outDocLn state
         $  text "mode  = " 
         <> text (show   $ Set.toList 
                         $ stateModes state)
        
        outDocLn state
         $  text "trans = "
         <> ppr (stateTransform state)

        return state

-- Toggle active modes.
cmdSet state cmd
 | ["lang", name]       <- words cmd
 = do   case lookup name stateProfiles of
         Just profile   
          -> do putStrLn "ok"
                return $ state { stateProfile = profile }

         Nothing
          -> do putStrLn "unknown language fragment"
                return state

 | "trans" : rest       <- words cmd
 = do   case parseTransform (concat rest) of
         Just spec       
          -> do putStrLn "ok"
                return $ state { stateTransform = spec }

         Nothing
          -> do putStrLn "transform spec parse error"
                return state

 | ("rule", rest)	<- R.parseFirstWord cmd
 = case R.parseRewrite rest of
	Right (R.SetAdd name rule)
	 -> do	putStrLn $ "ok, added " ++ name
		let rules = stateRewriteRules state
		let rules'= Map.insert name rule rules
		return $ state { stateRewriteRules = rules' }

	Right (R.SetRemove name)
	 -> do	putStrLn $ "ok, removed " ++ name
		let rules = stateRewriteRules state
		let rules'= Map.delete name rules
		return $ state { stateRewriteRules = rules' }

	Right (R.SetList)
	 -> do	let rules = Map.toList $ stateRewriteRules state
		mapM_ (uncurry $ R.showRule state 0) rules
		return state
	
	Left e
	 -> do	putStrLn e
		return state

 | otherwise
 = case parseModeChanges cmd of
        Just changes
         -> do  let state'  = foldr (uncurry adjustMode) state changes
                putStrLn "ok"
                return state'
        
        Nothing
         -> do  putStrLn "mode parse error"
                return state

-- | Parse a string of mode changes.
parseModeChanges :: String -> Maybe [(Bool, Mode)]
parseModeChanges str
        = sequence $ map parseModeChange $ words str


-- | Parse a mode change setting.
--   "Mode" or "+Mode" to enable. "-Mode" to disable.
parseModeChange  :: String -> Maybe (Bool, Mode)
parseModeChange str
 = case str of
        ('+' : strMode)
         | Just mode    <- parseMode strMode
         -> Just (True, mode)
        
        ('-' : strMode)
         | Just mode    <- parseMode strMode
         -> Just (False, mode)

        (c : strMode)
         | isUpper c 
         , Just mode    <- parseMode (c : strMode)
         -> Just (True, mode)

        _ -> Nothing


