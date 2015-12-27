
module DDCI.Core.Command
        ( Command(..)
        , commands
        , readCommand
        , handleCmd)
where
import DDCI.Core.Command.Help
import DDCI.Core.Command.Set
import DDCI.Core.Command.TransInteract
import DDCI.Core.Command.With
import DDCI.Core.State
import DDCI.Core.Mode                   as Mode

import DDC.Driver.Command.Check
import DDC.Driver.Command.Load
import DDC.Driver.Command.Trans
import DDC.Driver.Command.Compile
import DDC.Driver.Command.ToSalt
import DDC.Driver.Command.ToC
import DDC.Driver.Command.ToLlvm
import DDC.Driver.Command.Tetra.Boxing
import DDC.Driver.Command.Flow.Prep
import DDC.Driver.Command.Flow.Rate
import DDC.Driver.Command.Flow.Lower
import DDC.Driver.Command.Flow.Concretize
import DDC.Driver.Command.Flow.Wind
import DDC.Driver.Command.Flow.Melt
import DDC.Driver.Command.Flow.Thread
import DDC.Driver.Command.Flow.ToTetra
import DDC.Type.Universe

import qualified DDC.Build.Interface.Store      as Store

import qualified DDC.Core.Flow                  as Flow
import qualified Data.Set                       as Set
import System.IO
import Control.Monad.Trans.Except
import Data.List


-- Command ----------------------------------------------------------------------------------------
-- | The commands that the interpreter supports.
data Command
        = CommandBlank          -- ^ No command was entered.
        | CommandUnknown        -- ^ Some unknown (invalid) command.
        | CommandHelp           -- ^ Display the interpreter help.
        | CommandSet            -- ^ Set a mode.
        | CommandLoad           -- ^ Load a module.
        | CommandSort           -- ^ Show the sort of a kind.
        | CommandKind           -- ^ Show the kind of a spec.
        | CommandEquivType      -- ^ Check if two types are equivalent.
        | CommandWitType        -- ^ Show the type of a witness.
        | CommandExpCheck       -- ^ Check the type of an expression.
        | CommandExpSynth       -- ^ Synthesize the type of an expression, including existentials.
        | CommandExpType        -- ^ Check an expression, showing its type.
        | CommandExpEffect      -- ^ Check an expression, showing its effect.
        | CommandExpClosure     -- ^ Check an expression, showing its closure.
        | CommandExpRecon       -- ^ Reconstruct type annotations on binders.

        -- Generic transformations
        | CommandTrans          -- ^ Transform an expression.
        | CommandTransInteract  -- ^ Interactively transform an expression.

        -- Make and compile
        | CommandCompile        -- ^ Compile a file.
        | CommandMake           -- ^ Compile and link and executable.

        -- Conversion to machine code
        | CommandToSalt         -- ^ Convert a module to Disciple Salt.
        | CommandToC            -- ^ Convert a module to C code.
        | CommandToLlvm         -- ^ Convert a module to LLVM code.

        -- Core Tetra specific passes.
        | CommandTetraBoxing            -- ^ Manage boxing of numeric types.

        -- Core Flow specific passes 
        | CommandFlowRate       -- ^ Perform rate inference

        | CommandFlowRateLower Flow.Config 
                                -- ^ Perform rate inference, followed by lowering

        | CommandFlowPrep       -- ^ Prepare a Core Flow module for lowering.

        | CommandFlowLower Flow.Config  
                                -- ^ Prepare and Lower a Core Flow module.

        | CommandFlowConcretize -- ^ Convert operations on type level rates to concrete ones.

        | CommandFlowMelt       -- ^ Melt compound data structures.

        | CommandFlowWind       -- ^ Wind loop primops into tail recursive loops.

        | CommandFlowThread     -- ^ Thread a world token through lowered code.

        | CommandFlowToTetra Flow.Config
                                -- ^ Convert to Disciple Tetra, which can then be converted to Salt

        -- Inline control
        | CommandWith           -- ^ Add a module to the inliner table.
        | CommandWithSalt
        deriving (Eq, Show)


---------------------------------------------------------------------------------------------------
-- | Names used to invoke each command.
--   Short names that form prefixes of other ones must come later
--   in the list. Eg ':with-lite' after ':with'
commands :: [(String, Command)]
commands 
 =      [ (":help",             CommandHelp)
        , (":?",                CommandHelp)
        , (":set",              CommandSet)
        , (":load",             CommandLoad)
        , (":sort",             CommandSort)
        , (":kind",             CommandKind)
        , (":tequiv",           CommandEquivType)
        , (":wtype",            CommandWitType)
        , (":check",            CommandExpCheck)
        , (":synth",            CommandExpSynth)
        , (":recon",            CommandExpRecon)

        , (":type",             CommandExpType)
        , (":effect",           CommandExpEffect)
        , (":closure",          CommandExpClosure)

        -- Generic transformations
        , (":tinteract",        CommandTransInteract)
        , (":trans",            CommandTrans)

        -- Conversion to machine code.
        , (":to-salt",          CommandToSalt)
        , (":to-c",             CommandToC)
        , (":to-llvm",          CommandToLlvm) 

        -- Core Tetra specific passes
        , (":tetra-boxing",     CommandTetraBoxing)

        -- Core Flow specific passes
        , (":flow-rate-lower",   CommandFlowRateLower Flow.defaultConfigScalar)
        , (":flow-rate",         CommandFlowRate)
        , (":flow-prep",         CommandFlowPrep)
        , (":flow-lower-kernel", CommandFlowLower Flow.defaultConfigKernel)
        , (":flow-lower-vector", CommandFlowLower Flow.defaultConfigVector)
        , (":flow-lower",        CommandFlowLower Flow.defaultConfigScalar)
        , (":flow-concretize",   CommandFlowConcretize)
        , (":flow-melt",         CommandFlowMelt)
        , (":flow-wind",         CommandFlowWind)
        , (":flow-thread",       CommandFlowThread)
        , (":flow-tetra",        CommandFlowToTetra Flow.defaultConfigScalar)

        -- Make and Compile
        , (":compile",          CommandCompile)
        , (":make",             CommandMake)

        -- Inliner control
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


-- Commands ---------------------------------------------------------------------------------------
-- | Handle a single line of input.
handleCmd :: State -> Command -> Source -> String -> IO State
handleCmd state CommandBlank _ _
 = return state

handleCmd state cmd source line
 = do   state'  <- handleCmd1 state cmd source line
        return state'

handleCmd1 state cmd source line
 = let  lang            = stateLanguage state
        traceCheck      = Set.member TraceCheck (stateModes state)
   in case cmd of
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
         -> do  configDriver    <- getDriverConfigOfState state
                runError 
                 $ cmdLoadCoreFromString 
                        configDriver
                        (stateLanguage state) source line
                return state

        CommandSort
         -> do  cmdShowType  lang UniverseKind source line
                return state

        CommandKind       
         -> do  cmdShowType  lang UniverseSpec source line
                return state

        CommandEquivType
         -> do  cmdTypeEquiv lang source line
                return state

        CommandWitType    
         -> do  cmdShowWType lang source line
                return state

        CommandExpCheck   
         -> do  cmdShowSpec  lang ShowSpecAll     False traceCheck source line
                return state

        CommandExpType  
         -> do  cmdShowSpec  lang ShowSpecData    False traceCheck source line
                return state

        CommandExpEffect  
         -> do  cmdShowSpec  lang ShowSpecEffect  False traceCheck source line
                return state

        CommandExpClosure 
         -> do  cmdShowSpec  lang ShowSpecClosure False traceCheck source line
                return state

        CommandExpSynth
         -> do  cmdShowSpec  lang ShowSpecAll     True  traceCheck source line
                return state

        CommandExpRecon
         -> do  cmdExpRecon  lang source line
                return state

        -- Generic transformations --------------
        CommandTrans
         -> do  configDriver    <- getDriverConfigOfState state
                runError
                 $ cmdTransDetect
                        configDriver
                        (stateLanguage state)
                        (Set.member Mode.TraceTrans $ stateModes state)
                        source line
                return state
        
        CommandTransInteract
         -> do  cmdTransInteract state source line
        

        -- Conversion to machine code -----------
        CommandToSalt
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToSaltCoreFromString config lang source line
                return    state

        CommandToC
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToSeaCoreFromString  config lang source line
                return state

        CommandToLlvm
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdToLlvmCoreFromString config lang source line
                return state

        -- Core Tetra specific passes -----------
        CommandTetraBoxing
         -> do  configDriver  <- getDriverConfigOfState state
                runError $ cmdTetraBoxing configDriver source line
                return state

        -- Core Flow specific passes ------------
        CommandFlowRate
         -> do  configDriver  <- getDriverConfigOfState state
                runError $ cmdFlowRate configDriver source line
                return state

        CommandFlowRateLower configLower
         -> do  configDriver  <- getDriverConfigOfState state
                runError $ cmdFlowRateLower configDriver configLower source line
                return state

        CommandFlowPrep
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowPrep config source line
                return state

        CommandFlowLower configLower
         -> do  configDriver   <- getDriverConfigOfState state
                runError $ cmdFlowLower configDriver configLower source line
                return state

        CommandFlowConcretize
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowConcretize config source line
                return state

        CommandFlowMelt
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowMelt config source line
                return state

        CommandFlowWind
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowWind config source line
                return state

        CommandFlowThread
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowThread config source line
                return state

        CommandFlowToTetra configLower
         -> do  config  <- getDriverConfigOfState state
                runError $ cmdFlowToTetraCoreFromString config configLower lang source line
                return    state

        -- Make and Compile ---------------------
        CommandCompile
         -> do  config  <- getDriverConfigOfState state
                store   <- Store.new
                runError $ cmdCompile config False store line
                return state

        CommandMake
         -> do  config  <- getDriverConfigOfState state
                store   <- Store.new
                runError $ cmdCompile config True store line
                return state


        -- Inliner Control ----------------------
        CommandWith
         ->     cmdWith state source line

        CommandWithSalt
         ->     cmdWithSalt state source line


-- | Just print errors to stdout and continue the session.
runError :: ExceptT String IO a -> IO ()
runError m
 = do   result  <- runExceptT m
        case result of
         Left err       -> hPutStrLn stdout err
         Right _        -> return ()

