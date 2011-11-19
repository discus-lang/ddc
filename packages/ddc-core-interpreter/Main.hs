
import DDCI.Core.Help
import System.IO

main :: IO ()
main 
 = do   putStrLn "DDCi-core, version 0.4.0: http://disciple.ouroborus.net  :? for help"

        -- Setup terminal mode.
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        loop


-- | The main REPL loop.
loop :: IO ()
loop
 = do   putStr "> "
        hFlush stdout
        line    <- getInput []
        putChar '\n'
        hFlush stdout
        handle line (words line)


-- | Handle an input line.
handle :: String -> [String] -> IO ()
handle _line ws
        | []    <- ws
        =       loop
        
        | ":quit" : _   <- ws
        =       return ()
        
        | cmd : _   <- ws
        , cmd == ":help" || cmd == ":?"
        = do    putStr help
                loop
        
        -- Some command we don't handle
        | cmd@(':' : _ ) : _       <- ws
        = do    putStrLn $ "unknown command '" ++ cmd ++ "'"
                putStrLn $ "use :? for help."
                loop
                
        -- An expression to evaluate
        | otherwise
        = do    putStrLn "*** This doesn't do anything yet"
                loop


-- | Get an input line from the console.
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
