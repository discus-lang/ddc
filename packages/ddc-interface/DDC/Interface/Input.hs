
module DDC.Interface.Input
        ( InputInterface (..)
        , InputState     (..)
        , Input          (..)
        , readInput)
where
import Data.List
import Data.Char


-- InputState -----------------------------------------------------------------
-- Interpreter input state
data InputState command
        = InputState
        { 
          -- Input mode.
          inputMode        :: Input

          -- Command that we're still receiving input for,
          -- along with the line number it started on.
        , inputCommand      :: Maybe (Maybe command, Int)


          -- The current line number in the command stream.
        , inputLineNumber   :: Int

          -- Accumulation of current input buffer.
        , inputAcc         :: String }


-- InputInterface -------------------------------------------------------------
-- | What interface is being used.
data InputInterface
        -- | Read commands from unix command-line args.
        = InputInterfaceArgs

        -- | Read commands interactively from the console.
        | InputInterfaceConsole

        -- | Read commands from the file with this name.
        | InputInterfaceBatch    FilePath
        deriving (Eq, Show)


-- Input ----------------------------------------------------------------------
-- | How we're reading the current expression.
data Input
        -- | Read input line-by-line, using a backslash at the end of the
        --   line to continue to the next.
        = InputLine

        -- | Read input as a block terminated by a double semicolon (;;)
        | InputBlock

        -- | Read input from a file specified on the prompt
        | InputFile     FilePath
        deriving (Eq, Show)




-- | Read the input mode from the front of a string.
readInput :: String -> (Input, String)
readInput ss
        | isPrefixOf ".." ss
        = (InputBlock, drop 2 ss)

        | isPrefixOf "<" ss
        , filePath      <- dropWhile isSpace (drop 1 ss)
        = (InputFile filePath, drop (length filePath) ss)

        | otherwise
        = (InputLine, ss)


