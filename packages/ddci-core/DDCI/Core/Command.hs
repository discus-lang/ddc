
module DDCI.Core.Command
        ( Command(..)
        , commands
        , readCommand
        , handleCmd)
where
import DDCI.Core.Command.Help
import DDCI.Core.Command.Set
import DDCI.Core.Command.Load
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import DDCI.Core.Command.Trans
import DDCI.Core.Command.Ast
import DDCI.Core.Command.Compile
import DDCI.Core.Command.Make
import DDCI.Core.Command.ToSalt
import DDCI.Core.Command.ToC
import DDCI.Core.Command.ToLlvm
import DDCI.Core.State
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
        | CommandAst            -- ^ Show the AST of an expression.

        | CommandCompile        -- ^ Compile a file.
        | CommandMake           -- ^ Compile and link and executable.

        | CommandToSalt         -- ^ Convert a module to Disciple Salt.
        | CommandToC            -- ^ Convert a module to C code.
        | CommandToLlvm         -- ^ Convert a module to LLVM code.
        deriving (Eq, Show)


-- | Names used to invoke each command.
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
        , (":trans",            CommandTrans)
        , (":trun",             CommandTransEval)
        , (":ast",              CommandAst) 
        , (":compile",          CommandCompile)
        , (":make",             CommandMake)
        , (":to-salt",          CommandToSalt)
        , (":to-c",             CommandToC)
        , (":to-llvm",          CommandToLlvm) ]


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
         -> do  cmdLoad state source line
                return state

        CommandKind       
         -> do  cmdShowKind state source line
                return state

        CommandUniverse
         -> do  cmdUniverse state source line
                return state

        CommandUniverse1
         -> do  cmdUniverse1 state source line
                return state

        CommandUniverse2
         -> do  cmdUniverse2 state source line
                return state

        CommandUniverse3
         -> do  cmdUniverse3 state source line
                return state

        CommandEquivType
         -> do  cmdTypeEquiv state source line
                return state

        CommandWitType    
         -> do  cmdShowWType state source line
                return state

        CommandExpCheck   
         -> do  cmdShowType state ShowTypeAll source line
                return state

        CommandExpType  
         -> do  cmdShowType state ShowTypeValue source line
                return state

        CommandExpEffect  
         -> do  cmdShowType state ShowTypeEffect source line
                return state

        CommandExpClosure 
         -> do  cmdShowType state ShowTypeClosure source line
                return state

        CommandExpRecon
         -> do  cmdExpRecon state source line
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
        
        CommandAst
         -> do  cmdAst state source line
                return state

        CommandToSalt
         -> do  cmdToSalt state source line
                return    state

        CommandToC
         -> do  cmdToC state source line
                return state

        CommandToLlvm
         -> do  cmdToLlvm state source line
                return state

        CommandCompile
         -> do  cmdCompile state source line
                return state

        CommandMake
         -> do  cmdMake state source line
                return state

