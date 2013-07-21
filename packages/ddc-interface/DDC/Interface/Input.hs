
module DDC.Interface.Input
        ( InputState    (..)
        , Input         (..)
        , readInput)
where
import Data.List
import Data.Char


-- InputState ----------------------------------------------------------------------
-- Interpreter input state
data InputState command
        = InputState
        { -- Command that we're still receiving input for,
          -- along with the line number it started on.
          inputCommand      :: Maybe (command, Int)

          -- Input mode.
        , _inputMode        :: Input

          -- The current line number in the command stream.
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


