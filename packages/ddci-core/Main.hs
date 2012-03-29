
import DDCI.Core.State
import DDCI.Core.Build.Make
import DDCI.Core.Command
import System.Environment
import Data.List
import Data.Maybe
import qualified System.Console.Haskeline       as HL
import qualified System.Console.Haskeline.IO    as HL


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         []      -> runInteractive

         -- Run a Disciple-Core-Exchange file.
         [filePath]
          | isSuffixOf ".dcx" filePath
          -> do file    <- readFile filePath
                runBatch filePath file

         ["--batch", filePath]
          -> do file    <- readFile filePath
                runBatch filePath file

         -- Make a file, depending on what the extension is.
         ["--make",  filePath]
          -> do let state       = initState (InputBatch filePath)
                makeFile state filePath
        
         _       -> runArgs args



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
-- | Run an interactive session, reading commands from the console.
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.3.0: http://disciple.ouroborus.net.   :? for help"

        -- Setup terminal mode.
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = do   hlState         <- HL.initializeInput HL.defaultSettings
        let state       = initState InputInteractive
        let inputState  = InputState Nothing InputLine 1 []
        loop state inputState hlState
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
-- | Run in batch mode, reading commands from the given string.
runBatch :: FilePath -> String -> IO ()
runBatch filePath str
 = do   let state       = initState (InputBatch filePath)
        let inputState  = InputState Nothing InputLine 1 []
        loop state inputState (lines str)
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


-- Args -----------------------------------------------------------------------
-- | Run in unix command-line mode, reading commands from a list of arguments.
runArgs :: [String] -> IO ()
runArgs args
 = do   let state    = initState InputArgs
        loop state args
 where 
        -- No more args, we're done.
        loop _state []
         = do   return ()

        loop state (('-':cmdColon) : file : rest)
         | isSuffixOf ":" cmdColon
         , cmdStr               <- init cmdColon
         , Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   contents        <- readFile file
                state'          <- handleCmd state cmd 0 contents
                loop state' rest

        loop state (('-':cmdStr) : arg : rest)
         | Just (cmd, [])       <- readCommand (':' : cmdStr)
         = do   state'          <- handleCmd state cmd 0 arg
                loop state' rest

        loop _state xs
         = error $ "bad args " ++ (show xs)


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




