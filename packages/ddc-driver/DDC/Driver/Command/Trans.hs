{-# OPTIONS -Werror #-}

module DDC.Driver.Command.Trans
        ( cmdTransDetect
        , cmdTransModule
        , cmdTransExp
        , cmdTransExpCont
        , transExp)
where
import DDC.Driver.Config
import DDC.Driver.Output
import DDC.Driver.Command.Check
import DDC.Driver.Interface.Source
import DDC.Build.Language
import DDC.Build.Pipeline
import DDC.Core.Transform.Reannotate
import DDC.Core.Load
import DDC.Core.Fragment
import DDC.Core.Simplifier
import DDC.Core.Exp.Annot
import DDC.Type.Exp.Simple
import DDC.Base.Pretty
import DDC.Base.Name
import DDC.Core.Module
import Data.Typeable
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import DDC.Type.Env                             as Env
import qualified DDC.Core.Check                 as C
import qualified Control.Monad.State.Strict     as S
import qualified DDC.Core.Env.EnvT              as EnvT
import Prelude                                  hiding ((<$>))


-- Detect -----------------------------------------------------------------------------------------
-- | Load and transform a module or expression, and print the result to @stdout@.
--  
--   If the source starts with the 'module' keyword then treat it as one,
--   otherwise treat it as an expression.
--
cmdTransDetect 
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Bool                 -- ^ Print transform info.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Input text.
        -> ExceptT String IO ()

cmdTransDetect    config language shouldPrintInfo
        source str

 | "module" : _ <- words str
 = cmdTransModule config language shouldPrintInfo
        source str

 | otherwise
 = cmdTransExp    config language shouldPrintInfo
        source str


-- Module -----------------------------------------------------------------------------------------
-- | Load and transform a module, and print the result to @stdout@.
cmdTransModule
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Language definition.
        -> Bool                 -- ^ Print transform info.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Input text.
        -> ExceptT String IO ()

cmdTransModule config language _shouldPrintInfo source str
 | Language bundle      <- language
 , fragment             <- bundleFragment   bundle
 , simpl                <- bundleSimplifier bundle
 , zero                 <- bundleStateInit  bundle
 = let
        pmode   = prettyModeOfConfig $ configPretty config

        pipeTrans
         = pipeText (nameOfSource source) (lineStartOfSource source) str
         $ PipeTextLoadCore fragment 
                (if configInferTypes config then C.Synth else C.Recon) 
                SinkDiscard
         [  PipeCoreReannotate (\a -> a { annotTail = ()})
         [  PipeCoreSimplify  fragment zero simpl
         [  PipeCoreCheck     fragment C.Recon SinkDiscard
         [  PipeCoreOutput    pmode SinkStdout ]]]]
 
    in do
        errs    <- liftIO pipeTrans 
        case errs of
         [] -> return ()
         es -> throwE $ renderIndent $ vcat $ map ppr es


-- Exp --------------------------------------------------------------------------------------------
-- | Load and transfrom an expression
--   and print the result to @stdout@.
cmdTransExp
        :: Config               -- ^ Driver config.
        -> Language             -- ^ Source language.
        -> Bool                 -- ^ Print transform info.
        -> Source               -- ^ Source of input text.
        -> String               -- ^ Input text.
        -> ExceptT String IO ()

cmdTransExp config language traceTrans
        source str
 
 = liftIO 
 $ cmdTransExpCont config traceTrans language 
        (\_ -> return ()) 
        source str


-- Cont -------------------------------------------------------------------------------------------
-- | Load an expression and apply the current transformation.
cmdTransExpCont
        :: Config       -- ^ Driver config.
        -> Bool 
        -> Language 
        -> (forall n. Typeable n
              => Exp (AnTEC () n) n -> IO ())
        -> Source 
        -> String 
        -> IO ()

cmdTransExpCont _config traceTrans language eatExp source str
 | Language bundle      <- language
 , fragment             <- bundleFragment   bundle
 , modules              <- bundleModules    bundle
 , simpl                <- bundleSimplifier bundle
 , zero                 <- bundleStateInit  bundle
 , profile              <- fragmentProfile  fragment
 =   cmdParseCheckExp fragment modules Recon False False source str 
 >>= goStore profile modules zero simpl
 where
        -- Expression is well-typed.
        goStore profile modules zero simpl (Just x, _)
         = do   let kenv    = modulesExportTypes  modules (profilePrimKinds profile)
                let tenv    = modulesExportValues modules (profilePrimTypes profile)

                tr      <- transExp traceTrans profile kenv tenv zero simpl 
                        $  reannotate (\a -> a { annotTail = ()}) x
                
                case tr of
                  Nothing -> return ()
                  Just x' 
                   -> do outDocLn $ ppr x'
                         eatExp x'

        -- Expression had a parse or type error.
        goStore _ _ _ _ _
         = do   return ()


-- Trans ------------------------------------------------------------------------------------------
-- | Transform an expression, or display errors
transExp
        :: (Ord n, Pretty n, Show n, CompoundName n)
        => Bool                         -- ^ Trace transform information.
        -> Profile n                    -- ^ Language profile.
        -> KindEnv n                    -- ^ Kind Environment.
        -> TypeEnv n                    -- ^ Type Environment.
        -> s
        -> Simplifier s (AnTEC () n) n
        -> Exp (AnTEC () n) n
        -> IO (Maybe (Exp (AnTEC () n) n))

transExp traceTrans profile kenv tenv zero simpl xx
 = do
        let annot  = annotOfExp xx
        let t1     = annotType    annot
        let eff1   = annotEffect  annot

         -- Apply the simplifier.
        let tx  = flip S.evalState zero
                $ applySimplifierX profile kenv tenv simpl xx
        
        let x'  = reannotate (const ()) $ result tx

        when (traceTrans)
         $ case (resultInfo tx) of
           TransformInfo inf
            -> outDocLn  
            $  text "* TRANSFORM INFORMATION: " <$> indent 4 (ppr inf) <$> text ""

        let config = C.configOfProfile profile
        let rr     = C.checkExp config kenv tenv Recon C.DemandNone x' 

        -- Check that the simplifier perserved the type of the expression.
        case fst rr of
          Right (x2, t2, eff2)
           |  equivT    EnvT.empty t1 t2
           ,  subsumesT EnvT.empty kEffect eff1 eff2
           -> do return (Just x2)

           | otherwise
           -> do outDocLn $ vcat
                    [ text "* CRASH AND BURN: Transform is not type preserving."
                    , ppr x'
                    , text ":: 1 " <+> ppr t1
                    , text ":: 2 " <+> ppr t2
                    , text ":!:1 " <+> ppr eff1
                    , text ":!:2 " <+> ppr eff2 ]
                 return Nothing

          Left err
           -> do outDocLn $ vcat
                    [ text "* CRASH AND BURN: Type error in transformed program."
                    , ppr err
                    , text "" ]

                 outDocLn $ text "Transformed expression:"
                 outDocLn $ ppr x'
                 return Nothing
