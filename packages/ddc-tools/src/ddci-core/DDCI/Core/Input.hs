
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
eatLine :: State 
        -> InputState Command -> String 
        -> IO (State, InputState Command)

eatLine state (InputState mCommand inputMode lineNumber acc) line
 | Just _ <- stateTransInteract state
 = do
	state' <- cmdTransInteractLoop state line
	return (state', InputState mCommand inputMode (lineNumber+1) acc)

 | otherwise
 = do   
        -- If this is the first line then try to read the command and
        --  input mode from the front so we know how to continue.
        -- If we can't read an explicit command then assume this is 
        --  an expression to evaluate.
        let (cmd, lineStart, (input, rest))
             = case mCommand of
                -- We haven't started a command yet.
                Nothing
                 -> case readCommand line of
                     Just (cmd', rest') -> (cmd',        lineNumber, readInput rest')
                     Nothing            -> (CommandEval, lineNumber, (InputLine, line))
                
                -- We've already started a command, and this is more input for it.
                Just (cmd', lineStart')
                 -> (cmd', lineStart', (inputMode, line))

        let source 
                -- We were instructed to read the program from a file.
                -- Report this file as the source location, independent
                -- of how we were instructed to read it.
                | InputFile filePath    <- input
                = SourceFile filePath

                -- The program was embedded in the command stream.
                | otherwise
                = case stateInterface state of
                        InterfaceArgs           -> SourceArgs
                        InterfaceConsole        -> SourceConsole lineStart
                        InterfaceBatch file     -> SourceBatch   file lineStart


        case input of
         -- For line-by-line mode, if the line ends with backslash then keep
         -- reading, otherwise run the command.
         -- We also convert the backslash to a newline so the source
         -- position comes out right in parser error messages.
         InputLine
          | not $ null rest
          , last rest == '\\'
          , Just initRest       <- takeInit rest
          -> do return ( state
                       , InputState (Just (cmd, lineStart)) input
                                (lineNumber + 1)
                                (acc ++ initRest ++ "\n"))

          | otherwise
          -> do state'  <- handleCmd state cmd source (acc ++ rest)
                return ( state'
                       , InputState Nothing InputLine
                                (lineNumber + 1)
                                [])


         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                state'  <- handleCmd state cmd source (acc ++ rest')
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
         InputFile filePath
          -> do exists          <- doesFileExist filePath
                if exists 
                 then do        
                        contents  <- readFile filePath
                        state'    <- handleCmd state cmd source contents
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

