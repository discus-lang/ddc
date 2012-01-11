
import DDCI.Core.Command.Help
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import System.IO
import System.Environment
import Data.List
import Data.Maybe
import Control.Monad

import qualified System.Console.Readline as R


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
type State
        = (Maybe Command, Input, String)


-- Interactive ----------------------------------------------------------------
-- | Run an interactive session
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.4.0: http://disciple.ouroborus.net  :? for help"

        -- Setup terminal mode.
	R.initialize
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = loop (Nothing, InputLine, []) []
 where  
        loop state@(mCommand, _, _) history
         = do   -- If this isn't the first line then print the prompt.
		let prompt = if isNothing mCommand then "> " else ""
         
                -- Read a line from the user and echo it back.
                line    <- getInput prompt

                if isPrefixOf ":quit" line
                 then return ()
                 else do
                        state'@(mCommand', _, _)  <- eatLine state line

			-- Record entire input block in history
			let acc = history ++ "\n" ++ line
			history' <- if isNothing mCommand'
				then R.addHistory (tail acc) >> return []
				else return acc

                        loop state' history'


-- | Get an input line from the console, using given prompt
getInput :: String -> IO String
getInput prompt
 = do	line <- R.readline prompt
	return (fromMaybe ":quit" line)


-- Batch ----------------------------------------------------------------------
runBatch :: String -> IO ()
runBatch str
 = loop (lines str) (Nothing, InputLine, [])
 where 
        -- No more lines, we're done.
        -- There might be a command in the buffer though.
        loop [] state
         = do   eatLine state []
                return ()

        loop (l:ls) state
         -- Echo comment lines back.
         | isPrefixOf "--" l
         = do   putStrLn l
                loop ls state

         -- Quit the program.
         | isPrefixOf ":quit" l
         = do   return ()

         -- Handle a line of input.
         | otherwise
         = do   state'  <- eatLine state l
                loop ls state'


-- Eat ------------------------------------------------------------------------
-- Eating input lines.

eatLine :: State -> String -> IO State
eatLine (mCommand, inputMode, acc) line
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
          ->    return (Just cmd, input, acc ++ init rest)

          | otherwise
          -> do handleCmd cmd (acc ++ rest)
                return (Nothing, InputLine, [])

         -- For block mode, if the line ends with ';;' then run the command,
         -- otherwise keep reading.
         InputBlock
          | isSuffixOf ";;" rest
          -> do let rest' = take (length rest - 2) rest
                handleCmd cmd (acc ++ rest')
                return (Nothing, InputLine, [])

          | otherwise
          ->    return (Just cmd, input, acc ++ rest)


-- Commands -------------------------------------------------------------------
-- | Handle a single line of input.
handleCmd :: Command -> String -> IO ()
handleCmd CommandBlank _
 = return ()

handleCmd cmd line
 = do   handleCmd1 cmd line
        putStr "\n"

handleCmd1 cmd line
 = case cmd of
        CommandBlank
         -> return ()

        CommandUnknown
         -> do  putStr $ unlines
                 [ "unknown command."
                 , "use :? for help." ]

        CommandHelp
         -> do  putStr help

        CommandKind       -> cmdShowKind      line
        CommandWitType    -> cmdShowWType     line

        CommandExpCheck   -> cmdShowType ShowTypeAll     line
        CommandExpType    -> cmdShowType ShowTypeValue   line
        CommandExpEffect  -> cmdShowType ShowTypeEffect  line
        CommandExpClosure -> cmdShowType ShowTypeClosure line

        CommandEval       -> cmdEval line
        
