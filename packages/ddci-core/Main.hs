
import DDCI.Core.State
import DDCI.Core.Command.Help
import DDCI.Core.Command.Set
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import System.IO
import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import qualified System.Console.Haskeline       as HL
import qualified System.Console.Haskeline.IO    as HL


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileName]
          -> do file    <- readFile fileName
                runBatch file
       
         _ -> runInteractive


-- Command --------------------------------------------------------------------
-- | The commands that the interpreter supports.
data Command
        = CommandBlank
        | CommandUnknown
        | CommandHelp
        | CommandSet
        | CommandKind
        | CommandWitType
        | CommandExpCheck
        | CommandExpType
        | CommandExpEffect
        | CommandExpClosure
        | CommandEval
        deriving (Eq, Show)


-- | Names used to invoke each command.
commands :: [(String, Command)]
commands 
 =      [ (":help",     CommandHelp)
        , (":?",        CommandHelp)
        , (":set",      CommandSet)
        , (":kind",     CommandKind)
        , (":wtype",    CommandWitType)
        , (":check",    CommandExpCheck)
        , (":type",     CommandExpType)
        , (":effect",   CommandExpEffect)
        , (":closure",  CommandExpClosure)
        , (":eval",     CommandEval) ]


-- | Read the command from the front of a string.
readCommand :: String -> Maybe (Command, String)
readCommand ss
        | null $ words ss
        = Just (CommandBlank,   ss)

        | [(cmd, rest)] <- [ (cmd, drop (length str) ss) 
                                        | (str, cmd)      <- commands
                                        , isPrefixOf str ss ]
        = Just (cmd, rest)

        | ':' : _       <- ss
        = Just (CommandUnknown, ss)

        | otherwise
        = Nothing


-- Input ----------------------------------------------------------------------
-- | How we're reading the input expression.
data Input
        -- | Read input line-by-line, using a backslash at the end of the
        --   line to continue to the next.
        = InputLine

        -- | Read input as a block terminated by a double semicolon (;;)
        | InputBlock
        deriving (Eq, Show)


-- | Read the input mode from the front of a string.
readInput :: String -> (Input, String)
readInput ss
        | isPrefixOf ".." ss
        = (InputBlock, drop 2 ss)

        | otherwise
        = (InputLine, ss)


-- State ----------------------------------------------------------------------
-- Interpreter state
type InputState
        = (Maybe Command, Input, String)


-- Interactive ----------------------------------------------------------------
-- | Run an interactive session
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.4.0: http://disciple.ouroborus.net  :? for help"

        -- Setup terminal mode.
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = do   hlState <- HL.initializeInput HL.defaultSettings
        loop initState (Nothing, InputLine, []) hlState
 where  
        loop state inputState@(mCommand, _, _) hlState 
         = do   -- If this isn't the first line then print the prompt.
		let prompt = if isNothing mCommand then "> " else ""
         
                -- Read a line from the user and echo it back.
                line    <- getInput hlState prompt

                if isPrefixOf ":quit" line
                 then return ()
                 else do
                        (state', inputState')
                                <- eatLine state inputState line

                        loop state' inputState' hlState


-- | Get an input line from the console, using given prompt
getInput :: HL.InputState -> String -> IO String
getInput hlState prompt
 = do	line <- HL.queryInput hlState (HL.getInputLine prompt)
	return (fromMaybe ":quit" line)


-- Batch ----------------------------------------------------------------------
runBatch :: String -> IO ()
runBatch str
 = loop initState (Nothing, InputLine, []) (lines str)
 where 
        -- No more lines, we're done.
        -- There might be a command in the buffer though.
        loop state inputState []
         = do   eatLine state inputState []
                return ()

        loop state inputState (l:ls)
         -- Echo comment lines back.
         | isPrefixOf "--" l
         = do   putStrLn l
                loop state inputState ls

         -- Quit the program.
         | isPrefixOf ":quit" l
         = do   return ()

         -- Handle a line of input.
         | otherwise
         = do   (state', inputState')  <- eatLine state inputState l
                loop state' inputState' ls


-- Eat ------------------------------------------------------------------------
-- Eating input lines.

eatLine :: State -> InputState -> String -> IO (State, InputState)
eatLine state (mCommand, inputMode, acc) line
 = do   -- If this is the first line then try to read the command and
        --  input mode from the front so we know how to continue.
        -- If we can't read an explicit command then assume this is 
        --  an expression to evaluate.
        let (cmd, (input, rest))
             = case mCommand of
                Nothing
                 -> case readCommand line of
                     Just (cmd', rest') -> (cmd',         readInput rest')
                     Nothing            -> (CommandEval, (InputLine, line))
                
                Just cmd'
                 -> (cmd', (inputMode, line))

        case input of
         -- For line-by-line mode, if the line ends with backslash then keep
         -- reading, otherwise run the command.
         InputLine
          | not $ null rest
          , last rest == '\\'
          ->    return (state, (Just cmd, input, acc ++ init rest))

          | otherwise
          -> do state'  <- handleCmd state cmd (acc ++ rest)
                return (state', (Nothing, InputLine, []))

         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                state'  <- handleCmd state cmd (acc ++ rest')
                return (state', (Nothing, InputLine, []))

          | otherwise
          ->    return (state, (Just cmd, input, acc ++ rest))


-- Commands -------------------------------------------------------------------
-- | Handle a single line of input.
handleCmd :: State -> Command -> String -> IO State
handleCmd state CommandBlank _
 = return state

handleCmd state cmd line
 = do   state'  <- handleCmd1 state cmd line
        putStr "\n"
        return state'

handleCmd1 state cmd line
 = case cmd of
        CommandBlank
         -> return state

        CommandUnknown
         -> do  putStr $ unlines
                 [ "unknown command."
                 , "use :? for help." ]

                return state

        CommandHelp
         -> do  putStr help
                return state

        CommandSet        
         -> do  state'  <- cmdSet line state
                return state'

        CommandKind       
         -> do  cmdShowKind      line
                return state

        CommandWitType    
         -> do  cmdShowWType     line
                return state

        CommandExpCheck   
         -> do  cmdShowType ShowTypeAll     line
                return state

        CommandExpType  
         -> do  cmdShowType ShowTypeValue   line
                return state

        CommandExpEffect  
         -> do  cmdShowType ShowTypeEffect  line
                return state

        CommandExpClosure 
         -> do  cmdShowType ShowTypeClosure line
                return state

        CommandEval       
         -> do  cmdEval state line
                return state
        
