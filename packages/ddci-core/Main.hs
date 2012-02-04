
import DDCI.Core.State
import DDCI.Core.Command.Help
import DDCI.Core.Command.Set
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import DDCI.Core.Command.Trans
import DDCI.Core.Command.Ast
import System.Environment
import Data.List
import Data.Maybe
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
        | CommandExpRecon
        | CommandExpType
        | CommandExpEffect
        | CommandExpClosure
        | CommandEval
        | CommandTrans
	| CommandAst
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
        , (":recon",    CommandExpRecon)
        , (":type",     CommandExpType)
        , (":effect",   CommandExpEffect)
        , (":closure",  CommandExpClosure)
        , (":eval",     CommandEval)
        , (":trans",    CommandTrans)
        , (":ast",	CommandAst) ]


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


-- InputState ----------------------------------------------------------------------
-- Interpreter input state
data InputState
        = InputState
        { -- Command that we're still receiving input for,
          -- along with the line number it started on.
          inputCommand     :: Maybe (Command, Int)

          -- Input mode.
        , _inputMode        :: Input

          -- Current line number, used for parse error messages.
        , inputLineNumber   :: Int

          -- Accumulation of current input buffer.
        , _inputAcc         :: String }


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


-- Interactive ----------------------------------------------------------------
-- | Run an interactive session
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.2.0: http://disciple.ouroborus.net  :? for help"

        -- Setup terminal mode.
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = do   hlState         <- HL.initializeInput HL.defaultSettings
        let inputState  = InputState Nothing InputLine 1 []
        loop initState inputState hlState
 where  
        loop state inputState hlState 
         = do   -- If this isn't the first line then print the prompt.
		let prompt = if isNothing (inputCommand inputState)
                                then "> "
                                else ""
         
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
 = do   let inputState  = InputState Nothing InputLine 1 []
        loop initState inputState (lines str)
 where 
        -- No more lines, we're done.
        -- There might be a command in the buffer though.
        loop state inputState []
         = do   eatLine state inputState []
                return ()

        loop state inputState (l:ls)
         -- Echo comment lines back.
         |  isPrefixOf "--" l
         = do   putStrLn l
                let inputState'
                        = inputState 
                        { inputLineNumber = inputLineNumber inputState + 1 }

                loop state inputState' ls

         -- Echo blank lines back if we're between commands.
         | Nothing      <- inputCommand inputState
         , null l
         = do   putStr "\n"
                let inputState'
                        = inputState 
                        { inputLineNumber = inputLineNumber inputState + 1 }

                loop state inputState' ls

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
eatLine state (InputState mCommand inputMode lineNumber acc) line
 = do   -- If this is the first line then try to read the command and
        --  input mode from the front so we know how to continue.
        -- If we can't read an explicit command then assume this is 
        --  an expression to evaluate.
        let (cmd, lineStart, (input, rest))
             = case mCommand of
                Nothing
                 -> case readCommand line of
                     Just (cmd', rest') -> (cmd',        lineNumber, readInput rest')
                     Nothing            -> (CommandEval, lineNumber, (InputLine, line))
                
                Just (cmd', lineStart')
                 -> (cmd', lineStart', (inputMode, line))

        case input of
         -- For line-by-line mode, if the line ends with backslash then keep
         -- reading, otherwise run the command.
         -- We also convert the backslash to a newline so the source
         -- position comes out right in parser error messages.
         InputLine
          | not $ null rest
          , last rest == '\\'
          -> do return ( state
                       , InputState (Just (cmd, lineStart)) input
                                (lineNumber + 1)
                                (acc ++ init rest ++ "\n"))

          | otherwise
          -> do state'  <- handleCmd state cmd lineStart (acc ++ rest)
                return ( state'
                       , InputState Nothing InputLine
                                (lineNumber + 1)
                                [])


         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                state'  <- handleCmd state cmd lineStart (acc ++ rest')
                return ( state'
                       , InputState Nothing InputLine
                                (lineNumber + 1)
                                [])

          | otherwise
          ->    return ( state
                       , InputState (Just (cmd, lineStart)) input
                                (lineNumber + 1)
                                (acc ++ rest ++ "\n"))


-- Commands -------------------------------------------------------------------
-- | Handle a single line of input.
handleCmd :: State -> Command -> Int -> String -> IO State
handleCmd state CommandBlank _ _
 = return state

handleCmd state cmd lineStart line
 = do   state'  <- handleCmd1 state cmd lineStart line
        return state'

handleCmd1 state cmd lineStart line
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
         -> do  cmdShowKind  lineStart line
                return state

        CommandWitType    
         -> do  cmdShowWType lineStart line
                return state

        CommandExpCheck   
         -> do  cmdShowType ShowTypeAll     lineStart line
                return state

        CommandExpType  
         -> do  cmdShowType ShowTypeValue   lineStart line
                return state

        CommandExpEffect  
         -> do  cmdShowType ShowTypeEffect  lineStart line
                return state

        CommandExpClosure 
         -> do  cmdShowType ShowTypeClosure lineStart line
                return state

        CommandExpRecon
         -> do  cmdExpRecon lineStart line
                return state

        CommandEval       
         -> do  cmdEval state lineStart line
                return state

        CommandTrans
         -> do  cmdTrans state lineStart line
                return state
        
        CommandAst
         -> do  cmdAst lineStart line
                return state
