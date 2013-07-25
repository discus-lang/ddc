
module DDCI.Tetra.Command
        ( Command (..)
        , commands
        , readCommand
        , handleCommand)
where
import DDCI.Tetra.State
import DDC.Interface.Source
import Data.List


data Command
        = CommandBlank          -- ^ No command was entered.
        | CommandUnknown        -- ^ Some unknown (invalid) command.
        | CommandHelp           -- ^ Display the interpreter help.
        deriving (Eq, Show)


-- | Names used to invoke each command.
commands :: [(String, Command)]
commands
 =      [ (":help",     CommandHelp)
        , (":?",        CommandHelp) ]


-- | Read the command from the front of a string.
readCommand :: String -> Maybe (Command, String)
readCommand ss
        | null $ words ss
        = Just (CommandBlank,   ss)

        | (cmd, rest) : _ 
                <- [ (cmd, drop (length str) ss) 
                        | (str, cmd)      <- commands
                        , isPrefixOf str ss ]
        = Just (cmd, rest)

        | ':' : _  <- ss
        = Just (CommandUnknown, ss)

        | otherwise
        = Nothing


handleCommand :: State -> Command -> Source -> String -> IO State
handleCommand state cmd source line
 = do   state'  <- handleCommand1 state cmd source line
        return state'

handleCommand1 state cmd _source _line
 = case cmd of
        CommandBlank
         -> return state

        CommandUnknown
         -> do  putStr $ unlines
                 [ "unknown command."
                 , "use :? for help." ]

                return state

        CommandHelp
         -> do  putStr "you feel content"
                return state

