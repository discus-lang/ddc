
module DDCI.Core.Input
        ( InputState(..)
        , Input (..) 
        , readInput
        , eatLine)
where
import DDCI.Core.State
import DDCI.Core.Command
import System.Directory
import Data.List
import Data.Char



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

        -- | Read input from a file specified on the prompt
        | InputFile
        deriving (Eq, Show)


-- | Read the input mode from the front of a string.
readInput :: String -> (Input, String)
readInput ss
        | isPrefixOf ".." ss
        = (InputBlock, drop 2 ss)

        | isPrefixOf "-" ss
        = (InputFile, drop 1 ss)

        | otherwise
        = (InputLine, ss)


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

         -- Read input from a file
         InputFile 
          -> do let filePath    = dropWhile isSpace rest
                exists          <- doesFileExist filePath
                if exists 
                 then do        
                        contents  <- readFile filePath
                        state'    <- handleCmd state cmd lineStart contents
                        return  ( state'
                                , InputState Nothing InputLine
                                        (lineNumber + 1)
                                        [])
                 else do
                        putStrLn "No such file."
                        return  ( state
                                , InputState Nothing InputLine
                                        (lineNumber + 1)
                                        [])

