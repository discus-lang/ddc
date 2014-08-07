
module DDCI.Tetra.Command
        ( Command (..)
        , commands
        , readCommand
        , handleCommand)
where
import DDCI.Tetra.State
import DDCI.Tetra.Command.Help
import DDCI.Tetra.Command.Set
import DDCI.Tetra.Command.Parse
import DDCI.Tetra.Command.Desugar
import DDCI.Tetra.Command.Infer
import DDCI.Tetra.Command.ToCore
import DDC.Interface.Source
import DDC.Driver.Command.ToSalt       
import Control.Monad.Trans.Except
import Data.List
import System.IO


-- | Commands accepted by ddci-tetra.
data Command
        = CommandBlank          -- ^ No command was entered.
        | CommandUnknown        -- ^ Some unknown (invalid) command.
        | CommandHelp           -- ^ Display the interpreter help.
        | CommandSet            -- ^ Set an interpreter mode.
        | CommandParse          -- ^ Parse a Tetra source module.
        | CommandDesugar        -- ^ Desugar a Tetra source module.
        | CommandInfer          -- ^ Perform type inference.
        | CommandToCore         -- ^ Convert to Core Tetra.
        | CommandToSalt         -- ^ Convert to Core Salt.
        deriving (Eq, Show)


-- | Names used to invoke each command.
commands :: [(String, Command)]
commands
 =      [ (":help",     CommandHelp)
        , (":?",        CommandHelp) 
        , (":set",      CommandSet)
        , (":parse",    CommandParse) 
        , (":desugar",  CommandDesugar)
        , (":infer",    CommandInfer) 
        , (":to-core",  CommandToCore) 
        , (":to-salt",  CommandToSalt) ]


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

handleCommand1 state cmd source line
 = case cmd of
        CommandBlank
         -> return state

        CommandUnknown
         -> do  putStr $ unlines
                 [ "unknown command."
                 , "use :? for help." ]

                return state

        CommandHelp
         -> do  putStrLn help
                return state

        CommandSet
         -> do  state'  <- cmdSet state line
                return state'

        CommandParse
         -> do  cmdParse state source line
                return state

        CommandDesugar
         -> do  cmdDesugar state source line
                return state

        CommandInfer
         -> do  cmdInfer state source line
                return state

        CommandToCore
         -> do  config  <- getDriverConfigOfState state
                cmdToCore state config source line
                return state

        CommandToSalt
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToSaltSourceTetraFromString config source line
                return state


-- | Just print errors to stdout and continue the session.
runError :: ExceptT String IO () -> IO ()
runError m
 = do   result  <- runExceptT m
        case result of
         Left err       -> hPutStrLn stdout err
         Right _        -> return ()

