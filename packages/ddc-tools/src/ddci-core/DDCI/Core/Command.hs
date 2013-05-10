
module DDCI.Core.Command
        ( Command(..)
        , commands
        , readCommand
        , handleCmd)
where
import DDCI.Core.Command.Help
import DDCI.Core.Command.Set
import DDCI.Core.Command.Eval
import DDCI.Core.Command.Trans
import DDCI.Core.Command.TransInteract
import DDCI.Core.Command.With
import DDCI.Core.State
import DDC.Driver.Command.Ast
import DDC.Driver.Command.Check
import DDC.Driver.Command.Load
import DDC.Driver.Command.Compile
import DDC.Driver.Command.Make
import DDC.Driver.Command.ToSalt
import DDC.Driver.Command.ToC
import DDC.Driver.Command.ToLlvm
import DDC.Driver.Command.FlowLower
import System.IO
import Control.Monad.Trans.Error
import Data.List


-- Command --------------------------------------------------------------------
-- | The commands that the interpreter supports.
data Command
        = CommandBlank          -- ^ No command was entered.
        | CommandUnknown        -- ^ Some unknown (invalid) command.
        | CommandHelp           -- ^ Display the interpreter help.
        | CommandSet            -- ^ Set a mode.
        | CommandLoad           -- ^ Load a module.
        | CommandKind           -- ^ Show the kind of a type.
        | CommandUniverse       -- ^ Show the universe of a type.
        | CommandUniverse1      -- ^ Given a type, show the universe of the original thing.
        | CommandUniverse2      -- ^ Given a kind, show the universe of the original thing.
        | CommandUniverse3      -- ^ Given a sort, show the universe of the original thing.
        | CommandEquivType      -- ^ Check if two types are equivalent.
        | CommandWitType        -- ^ Show the type of a witness.
        | CommandExpCheck       -- ^ Check an expression.
        | CommandExpType        -- ^ Check an expression, showing its type.
        | CommandExpEffect      -- ^ Check an expression, showing its effect.
        | CommandExpClosure     -- ^ Check an expression, showing its closure.
        | CommandExpRecon       -- ^ Reconstruct type annotations on binders.
        | CommandEval           -- ^ Evaluate an expression.
        | CommandTrans          -- ^ Transform an expression.
        | CommandTransEval      -- ^ Transform then evaluate an expression.
        | CommandTransInteract  -- ^ Interactively transform an expression.

        | CommandAst            -- ^ Show the AST of an expression.

        | CommandCompile        -- ^ Compile a file.
        | CommandMake           -- ^ Compile and link and executable.

        | CommandToSalt         -- ^ Convert a module to Disciple Salt.
        | CommandToC            -- ^ Convert a module to C code.
        | CommandToLlvm         -- ^ Convert a module to LLVM code.

        | CommandFlowLower      -- ^ Lower a Disciple Core Flow module.

        | CommandWith           -- ^ Add a module to the inliner table.
	| CommandWithLite
	| CommandWithSalt
        deriving (Eq, Show)


-- | Names used to invoke each command.
--   Short names that form prefixes of other ones must come later
--   in the list. Eg ':with-lite' after ':with'
commands :: [(String, Command)]
commands 
 =      [ (":help",             CommandHelp)
        , (":?",                CommandHelp)
        , (":set",              CommandSet)
        , (":load",             CommandLoad)
        , (":kind",             CommandKind)
        , (":universe1",        CommandUniverse1)
        , (":universe2",        CommandUniverse2)
        , (":universe3",        CommandUniverse3)
        , (":universe",         CommandUniverse)
        , (":tequiv",           CommandEquivType)
        , (":wtype",            CommandWitType)
        , (":check",            CommandExpCheck)
        , (":recon",            CommandExpRecon)
        , (":type",             CommandExpType)
        , (":effect",           CommandExpEffect)
        , (":closure",          CommandExpClosure)
        , (":eval",             CommandEval)
        , (":trun",             CommandTransEval)
        , (":tinteract",        CommandTransInteract)
        , (":trans",            CommandTrans)
        , (":ast",              CommandAst) 
        , (":compile",          CommandCompile)
        , (":make",             CommandMake)
        , (":to-salt",          CommandToSalt)
        , (":to-c",             CommandToC)
        , (":to-llvm",          CommandToLlvm) 
        , (":flow-lower",       CommandFlowLower)
        , (":with-lite",        CommandWithLite)
        , (":with-salt",        CommandWithSalt) 
        , (":with",             CommandWith) ]


-- | Read the command from the front of a string.
readCommand :: String -> Maybe (Command, String)
readCommand ss
        | null $ words ss
        = Just (CommandBlank,   ss)

        | (cmd, rest) : _ <- [ (cmd, drop (length str) ss) 
                                        | (str, cmd)      <- commands
                                        , isPrefixOf str ss ]
        = Just (cmd, rest)

        | ':' : _       <- ss
        = Just (CommandUnknown, ss)

        | otherwise
        = Nothing


-- Commands -------------------------------------------------------------------
-- | Handle a single line of input.
handleCmd :: State -> Command -> Source -> String -> IO State
handleCmd state CommandBlank _ _
 = return state

handleCmd state cmd source line
 = do   state'  <- handleCmd1 state cmd source line
        return state'

handleCmd1 state cmd source line
 = case cmd of
        CommandBlank
         -> return state

        CommandUnknown
         -> do  putStr $ unlines
                 [ "unknown command."
                 , "use :? for help." ]

                return state

        CommandHelp
         -> do  putStr help
                return state

        CommandSet
         -> do  state'  <- cmdSet state line
                return state'

        CommandLoad
         -> do  runError $ cmdLoadFromString (stateLanguage state) source line
                return state

        CommandKind       
         -> do  cmdShowKind  (stateLanguage state) source line
                return state

        CommandUniverse
         -> do  cmdUniverse  (stateLanguage state) source line
                return state

        CommandUniverse1
         -> do  cmdUniverse1 (stateLanguage state) source line
                return state

        CommandUniverse2
         -> do  cmdUniverse2 (stateLanguage state) source line
                return state

        CommandUniverse3
         -> do  cmdUniverse3 (stateLanguage state) source line
                return state

        CommandEquivType
         -> do  cmdTypeEquiv (stateLanguage state) source line
                return state

        CommandWitType    
         -> do  cmdShowWType (stateLanguage state) source line
                return state

        CommandExpCheck   
         -> do  cmdShowType  (stateLanguage state) ShowTypeAll source line
                return state

        CommandExpType  
         -> do  cmdShowType  (stateLanguage state) ShowTypeValue source line
                return state

        CommandExpEffect  
         -> do  cmdShowType  (stateLanguage state) ShowTypeEffect source line
                return state

        CommandExpClosure 
         -> do  cmdShowType  (stateLanguage state) ShowTypeClosure source line
                return state

        CommandExpRecon
         -> do  cmdExpRecon  (stateLanguage state) source line
                return state

        CommandEval       
         -> do  cmdEval state source line
                return state

        CommandTrans
         -> do  cmdTrans state source line
                return state
        
        CommandTransEval
         -> do  cmdTransEval state source line
                return state
        
        CommandTransInteract
         -> do  cmdTransInteract state source line
        
        CommandAst
         -> do  cmdAstExp (stateLanguage state) source line
                return state

        CommandToSalt
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToSalt config (stateLanguage state) source line
                return    state

        CommandToC
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToC    config (stateLanguage state) source line
                return state

        CommandToLlvm
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToLlvm config (stateLanguage state) source line
                return state

        CommandFlowLower
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowLower config source line
                return state

        CommandCompile
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdCompile config line
                return state

        CommandMake
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdMake config line
                return state

        CommandWith
         ->     cmdWith state source line

        CommandWithLite
         ->     cmdWithLite state source line

        CommandWithSalt
         ->     cmdWithSalt state source line


-- | Just print errors to stdout and continue the session.
runError :: ErrorT String IO () -> IO ()
runError m
 = do   result  <- runErrorT m
        case result of
         Left err       -> hPutStrLn stdout err
         Right _        -> return ()

