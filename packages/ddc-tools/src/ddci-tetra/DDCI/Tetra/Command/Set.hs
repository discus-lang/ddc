
module DDCI.Tetra.Command.Set
        (cmdSet)
where
import DDCI.Tetra.State
import DDCI.Tetra.Mode
import DDC.Build.Builder
import DDC.Base.Pretty
import Data.Char
import Data.List
import qualified Data.Set               as Set


cmdSet :: State -> String -> IO State

cmdSet state []
 = do   putDocLn RenderIndent
         $ vcat [ text "Modes:   " 
                        <> text (show $ Set.toList $ stateModes state) ]

        return state

cmdSet state cmd
 | "builder" : name : []        <- words cmd
 = do   config <- getDefaultBuilderConfig
        case find (\b -> builderName b == name) (builders config) of
         Nothing
          -> do putStrLn "unknown builder"
                return state

         Just builder
          -> do putStrLn "ok"
                return state { stateBuilder = Just builder }

 | otherwise
 = case parseModeChanges cmd of
        Just changes
         -> do  let state'      = foldr (uncurry adjustMode) state changes
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


