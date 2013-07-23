
module DDCI.Core.Input
        ( InputState    (..)
        , Input         (..) 
        , readInput
        , eatLine)
where
import DDCI.Core.State
import DDCI.Core.Command
import DDCI.Core.Command.TransInteract
import DDC.Interface.Input
import DDC.Data.ListUtils
import System.Directory
import Data.List


-- Eat ------------------------------------------------------------------------
-- Eating input lines.
eatLine :: State -> InputState Command 
        -> String 
        -> IO (State, InputState Command)

eatLine state inputState@(InputState mCommand inputMode lineNumber acc) chunk
 | Just _ <- stateTransInteract state
 = do   state' <- cmdTransInteractLoop state chunk
	return (state', InputState mCommand inputMode (lineNumber+1) acc)

 | otherwise 
 = do  (inputState', mCmdLine)    
                <- inputLine (stateInterface state) readCommand inputState chunk
   
       case mCmdLine of
         Nothing        
          ->    return  (state, inputState')

         Just (source, Nothing,  line)
          -> do state'  <- handleCmd state CommandEval source line
                return  (state', inputState')

         Just (source, Just cmd, line)
          -> do state'  <- handleCmd state cmd source line
                return  (state', inputState')         


-- TODO: define nicer data type for Maybe result.
-- TODO: shift Source into this package.
inputLine 
        :: InputInterface
        -> (String -> Maybe (c, String))
        -> InputState c 
        -> String
        -> IO ( InputState c
              , Maybe (Source, Maybe c, String))
                        -- Just for complete command.
                        --  .. then Maybe for the default command.

inputLine interface readCmd inputState chunk
 | InputState inputMode mCommand lineNumber acc <- inputState
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
                 -> (cmd', lineStart', (inputMode, chunk))

        let source 
                -- We were instructed to read the program from a file.
                -- Report this file as the source location, independent
                -- of how we were instructed to read it.
                | InputFile filePath    <- input
                = SourceFile filePath

                -- The program was embedded in the command stream.
                | otherwise
                = case interface of
                        InputInterfaceArgs           -> SourceArgs
                        InputInterfaceConsole        -> SourceConsole lineStart
                        InputInterfaceBatch file     -> SourceBatch   file lineStart


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
                                { inputCommand          =  (Just (cmd, lineStart))
                                , inputLineNumber       = lineNumber + 1
                                , inputAcc              = acc ++ initRest ++ "\n" }
                        , Nothing)

          | otherwise
          -> return     ( inputState
                                { inputMode             = InputLine
                                , inputCommand          = Nothing
                                , inputLineNumber       = lineNumber + 1
                                , inputAcc              = [] }
                        , Just (source, cmd, acc ++ rest))


         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                return  ( inputState
                                { inputMode             = InputLine
                                , inputCommand          = Nothing
                                , inputLineNumber       = lineNumber + 1
                                , inputAcc              = [] }
                       , Just (source, cmd, acc ++ rest'))

          | otherwise
          ->    return ( inputState
                                { inputMode             = input
                                , inputCommand          = Just (cmd, lineStart)
                                , inputLineNumber       = lineNumber + 1
                                , inputAcc              = acc ++ rest ++ "\n" }
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

