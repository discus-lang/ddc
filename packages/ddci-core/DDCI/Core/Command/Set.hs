
-- | Display and toggle active interpreter modes.
module DDCI.Core.Command.Set
        ( Mode(..)
        , cmdSet)
where
import DDCI.Core.State
import DDCI.Core.Mode
import Data.List
import Data.Char
import qualified Data.Set       as Set


cmdSet :: String -> State -> IO State

-- Display the active modes.
cmdSet [] state
 = do   putStrLn $ show
                 $ Set.toList 
                 $ stateModes state

        return state

-- Toggle active modes.
cmdSet cmd state
 | Just changes <- parseModeChanges cmd
 = do   let state'        = foldr (uncurry applyMode) state changes
        putStrLn "ok"
        return state'

 | otherwise
 = do   putStrLn "mode parse error"
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


