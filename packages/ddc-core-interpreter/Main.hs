
import DDCI.Core.Command.Help
import DDCI.Core.Command.Anon
import DDCI.Core.Command.Free
import DDCI.Core.Command.Check
import DDCI.Core.Command.Subst
import DDCI.Core.Command.Eval
import System.IO
import System.Environment
import Data.List
import Control.Monad


main :: IO ()
main 
 = do   args    <- getArgs
        case args of
         [fileName]
          -> do file    <- readFile fileName
                runBatch file
         
         _ -> runInteractive


-- Interactive ------------------------------------------------------------------------------------
-- | Run an interactive session
runInteractive :: IO ()
runInteractive
 = do   putStrLn "DDCi-core, version 0.4.0: http://disciple.ouroborus.net  :? for help"

        -- Setup terminal mode.
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        loopInteractive


-- | The main REPL loop.
loopInteractive :: IO ()
loopInteractive 
 = loop []
 where  loop acc
         = do   when (null acc)
                 $ do   putStr "> "
                        hFlush stdout
         
                line    <- getInput []
                putChar '\n'
                hFlush stdout

                case line of
                 []     -> doCmd acc
                 _
                  | last line == '\\'
                  -> loop  (acc ++ init line) 
          
                  | otherwise
                  -> doCmd (acc ++ line)

        doCmd [] = loop []
        doCmd cmd
         = do   continue  <- handleCmdLine cmd
                if continue
                 then loop []
                 else return ()


-- | Get an input line from the console.
--   TODO: We'd prefer to have proper readline support.
--         For now we just handle backspace.
getInput :: String -> IO String
getInput buf
 = do   c       <- hGetChar stdin
        getInput' c
 where
  getInput' c
        | c == '\n'
        = return (reverse buf)

        | _:bs  <- buf
        , c == '\DEL'
        = do    putStr "\b"
                putStr " "
                putStr "\b"
                hFlush stdout
                getInput bs
        
        | []    <- buf
        , c == '\DEL'
        = getInput []

        | otherwise
        = do    putStr [c]
                hFlush stdout
                getInput (c : buf)


-- Batch ------------------------------------------------------------------------------------------
runBatch :: String -> IO ()
runBatch str
 = loop (lines str) []
 where 
        loop []         acc   = doCmd [] acc
        loop ([]:ls) acc      = doCmd ls acc
        loop (line:ls) acc
            | last line == '\\'
            = loop ls (acc ++ init line) 
          
            | otherwise
            = doCmd ls (acc ++ line)

        doCmd [] [] = return ()
        doCmd ls cmd
         = do   continue  <- handleCmdLine cmd
                if continue
                 then loop ls []
                 else return ()


-- Commands ---------------------------------------------------------------------------------------
-- | Handle a single line of input.
handleCmdLine :: String -> IO Bool
handleCmdLine line
 = handleCmdLine1 line (words line)

-- | Handle an input line, and print newline after successful ones.
handleCmdLine1 :: String -> [String] -> IO Bool
handleCmdLine1 line ws
        -- Ignore empty lines.
        | []    <- ws
        =       return True

        -- Echo comment lines.
        | w:_   <- ws
        , isPrefixOf "--" w
        = do    putStr $ line ++ "\n"
                return True

        -- Quit the interpreter.
        | ":quit" : _   <- ws
        =       return False

        | otherwise
        = do    handled  <- handle line ws
                if handled
                 then do
                        putStr "\n"
                        return True
                 else do
                        putStrLn $ "unknown command."
                        putStrLn $ "use :? for help."
                        return True


-- | Handle an input line.
handle :: String -> [String] -> IO Bool
handle line ws
        -- Print the help screen.
        | cmd : _       <- ws
        , cmd == ":help" || cmd == ":?"
        = do    putStr help
                return True

        -- Anonymize --------------------------------------
        | Just rest     <- splitPrefix ":anonT" line
        = do    cmdAnonType rest
                return True
        
        -- Free -------------------------------------------
        | Just rest     <- splitPrefix ":freeT" line
        = do    cmdFreeType rest
                return True
        
        -- Subst ------------------------------------------
        | Just rest     <- splitPrefix ":substTT" line
        = do    cmdSubstTT rest
                return True
        
        -- Checking ---------------------------------------
        -- Show the kind of a type.
        | Just rest     <- splitPrefix ":kind" line
        = do    cmdShowKind rest
                return True

        -- Show the type of a witness.
        | Just rest     <- splitPrefix ":wtype" line
        = do    cmdShowWType rest
                return True

        -- Show the value type, effect and closure of an expression.
        | Just rest     <- splitPrefix ":check" line
        = do    cmdShowType ShowTypeAll rest
                return True 

        -- Show just the value type of an expression.
        | Just rest     <- splitPrefix ":type" line
        = do    cmdShowType ShowTypeValue rest
                return True

        -- Show just the effect of an expression.
        | Just rest     <- splitPrefix ":effect" line
        = do    cmdShowType ShowTypeEffect rest
                return True

        -- Show just the closure of an expression.
        | Just rest     <- splitPrefix ":closure" line
        = do    cmdShowType ShowTypeClosure rest
                return True
        
        -- Unknown ----------------------------------------
        -- Some command we don't handle.
        | (':' : _ ) : _       <- ws
        = do    return False
        
        -- An expression to evaluate.
        | otherwise
        = do    cmdEval line
                return True


-- | Split a prefix from the front of a string, returning the trailing part.
splitPrefix :: String -> String -> Maybe String
splitPrefix prefix str
        | isPrefixOf prefix str
        = Just $ drop (length prefix) str
        
        | otherwise
        = Nothing
        

