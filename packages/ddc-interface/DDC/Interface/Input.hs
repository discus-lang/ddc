
module DDC.Interface.Input
        ( InputInterface (..)
        , InputState     (..)
        , Input          (..)
        , readInput
        , inputLine)
where
import DDC.Interface.Source
import DDC.Data.ListUtils
import System.Directory
import Data.List
import Data.Char


-- InputState -----------------------------------------------------------------
-- Interpreter input state
data InputState command
        = InputState
        { -- Function to parse commands.
          inputParseCommand :: String -> Maybe (command, String)

          -- Input mode.
        , inputMode         :: Input

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

-------------------------------------------------------------------------------
inputLine 
        :: InputInterface
        -> InputState c 
        -> String
        -> IO ( InputState c
              , Maybe (Source, Maybe c, String))
                        -- Just for complete command.
                        --  .. then Maybe for the default command.

inputLine interface inputState chunk
 | InputState readCmd mode mCommand lineNumber acc <- inputState
 = do   
        -- If this is the first line then try to read the command and
        --  input mode from the front so we know how to continue.
        -- If we can't read an explicit command then assume this is 
        --  an expression to evaluate.
        let (cmd, lineStart, (input, rest))
             = case mCommand of
                -- We haven't started a command yet.
                Nothing
                 -> case readCmd chunk of
                     Just (cmd', rest') -> (Just cmd', lineNumber, readInput rest')
                     Nothing            -> (Nothing,   lineNumber, (InputLine, chunk))
                
                -- We've already started a command, and this is more input for it.
                Just (cmd', lineStart')
                 -> (cmd', lineStart', (mode, chunk))

        let source 
                -- We were instructed to read the program from a file.
                -- Report this file as the source location, independent
                -- of how we were instructed to read it.
                | InputFile filePath    <- input
                = SourceFile filePath

                -- The program was embedded in the command stream.
                | otherwise
                = case interface of
                        InputInterfaceArgs        -> SourceArgs
                        InputInterfaceConsole     -> SourceConsole lineStart
                        InputInterfaceBatch file  -> SourceBatch   file lineStart


        case input of
         -- For line-by-line mode, if the line ends with backslash then keep
         -- reading, otherwise run the command.
         -- We also convert the backslash to a newline so the source
         -- position comes out right in parser error messages.
         InputLine
          | not $ null rest
          , last rest == '\\'
          , Just initRest       <- takeInit rest
          -> return     ( inputState
                                { inputCommand    =  (Just (cmd, lineStart))
                                , inputLineNumber = lineNumber + 1
                                , inputAcc        = acc ++ initRest ++ "\n" }
                        , Nothing)

          | otherwise
          -> return     ( inputState
                                { inputMode       = InputLine
                                , inputCommand    = Nothing
                                , inputLineNumber = lineNumber + 1
                                , inputAcc        = [] }
                        , Just (source, cmd, acc ++ rest))


         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                return  ( inputState
                                { inputMode       = InputLine
                                , inputCommand    = Nothing
                                , inputLineNumber = lineNumber + 1
                                , inputAcc        = [] }
                       , Just (source, cmd, acc ++ rest'))

          | otherwise
          ->    return ( inputState
                                { inputMode       = input
                                , inputCommand    = Just (cmd, lineStart)
                                , inputLineNumber = lineNumber + 1
                                , inputAcc        = acc ++ rest ++ "\n" }
                       , Nothing)

         -- Read input from a file
         InputFile filePath
          -> do exists          <- doesFileExist filePath
                if exists 
                 then do        
                        contents  <- readFile filePath
                        return  ( inputState
                                        { inputMode     = InputLine
                                        , inputCommand  = Nothing
                                        , inputLineNumber = lineNumber + 1
                                        , inputAcc      = [] }
                                , Just (source, cmd, contents))
                 else do
                        putStrLn "No such file."
                        return  ( inputState
                                        { inputMode     = InputLine
                                        , inputCommand  = Nothing
                                        , inputLineNumber = lineNumber + 1
                                        , inputAcc      = [] }
                                , Nothing)


