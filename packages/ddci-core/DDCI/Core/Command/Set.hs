
-- | Display and toggle active interpreter modes.
module DDCI.Core.Command.Set
        ( Mode(..)
        , cmdSet)
where
import DDCI.Core.Language
import DDCI.Core.State
import DDCI.Core.Mode
import DDCI.Core.Output
import DDC.Core.Fragment.Profile
import DDC.Core.Simplifier
import DDC.Base.Pretty
import Data.Char
import qualified DDCI.Core.Rewrite as R
import qualified Data.Set       as Set
import qualified Data.Map       as Map


cmdSet ::  State -> String -> IO State

-- Display the active modes.
cmdSet state []
 = do   let langName
                = case stateLanguage state of
                   Language frag
                    -> profileName (fragmentProfile frag)

        putStrLn $ renderIndent
         $ vcat [ text "mode  = " <> text (show $ Set.toList 
                                                $ stateModes state)

                , text "lang  = " <> text langName

                , text "simpl = " <> ppr (stateSimplifier state) ]

        return state

-- Toggle active modes.
cmdSet state cmd
 | ["lang", name]       <- words cmd
 = do   case lookup name languages of
         Just profile   
          -> do chatStrLn state "ok"
                return $ state { stateLanguage = profile }

         Nothing
          -> do putStrLn "unknown language"
                return state

 | "trans" : rest       <- words cmd
 = do   case parseSimplifier (concat rest) of
         Just simpl
          -> do chatStrLn state "ok"
                return $ state { stateSimplifier = simpl }

         Nothing
          -> do putStrLn "transform spec parse error"
                return state

 | ("rule", rest)	<- R.parseFirstWord cmd
 = case R.parseRewrite rest of
	Right (R.SetAdd name rule)
	 -> do	chatStrLn state $ "ok, added " ++ name
		let rules = stateRewriteRules state
		let rules'= Map.insert name rule rules
		return $ state { stateRewriteRules = rules' }

	Right (R.SetRemove name)
	 -> do	chatStrLn state $ "ok, removed " ++ name
		let rules = stateRewriteRules state
		let rules'= Map.delete name rules
		return $ state { stateRewriteRules = rules' }

	Right (R.SetList)
	 -> do	let rules = Map.toList $ stateRewriteRules state
		mapM_ (uncurry $ R.showRule state 0) rules
		return state
	
	Left e
	 -> do	chatStrLn state e
		return state

 | "outputDir" : dir : []    <- words cmd
 = return $ state { stateOutputDir  = Just dir }

 | "output" : file : []      <- words cmd
 = return $ state { stateOutputFile = Just file }


 | otherwise
 = case parseModeChanges cmd of
        Just changes
         -> do  let state'  = foldr (uncurry adjustMode) state changes
                chatStrLn state "ok"
                return state'
        
        Nothing
         -> do  chatStrLn state "mode parse error"
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
         | Just mode    <- readMode strMode
         -> Just (True, mode)
        
        ('/' : strMode)
         | Just mode    <- readMode strMode
         -> Just (False, mode)

        (c : strMode)
         | isUpper c 
         , Just mode    <- readMode (c : strMode)
         -> Just (True, mode)

        _ -> Nothing


