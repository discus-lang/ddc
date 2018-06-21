
module DDC.Build.Stage.Core.Discus
        ( discusToSalt
        , ConfigDiscusToSalt     (..))
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import DDC.Data.Pretty

import qualified DDC.Build.Stage.Core                   as B
import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Language.Discus              as BD

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as CNamify
import qualified DDC.Core.Transform.Unshare             as CUnshare

import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Platform                 as A
import qualified DDC.Core.Salt.Runtime                  as A

import qualified DDC.Core.Discus                        as D
import qualified DDC.Core.Discus.Transform.Boxing       as DBoxing
import qualified DDC.Core.Discus.Transform.Curry        as DCurry
import qualified DDC.Core.Discus.Transform.Initialize   as DInitialize


---------------------------------------------------------------------------------------------------
data ConfigDiscusToSalt
        = ConfigDiscusToSalt
        { configSinkExplicit    :: B.Sink       -- ^ Sink after making explicit.
        , configSinkInitialize  :: B.Sink       -- ^ Sink after initialize transform.
        , configSinkLambdas     :: B.Sink       -- ^ Sink after lambda lifting.
        , configSinkUnshare     :: B.Sink       -- ^ Sink after unsharing.
        , configSinkCurry       :: B.Sink       -- ^ Sink after curry transform.
        , configSinkBoxing      :: B.Sink       -- ^ Sink after boxing transform.
        , configSinkPrep        :: B.Sink       -- ^ Sink after prep before to-salt conversion.
        , configSinkChecked     :: B.Sink       -- ^ Sink after checking before to-salt converion.
        , configSinkSalt        :: B.Sink       -- ^ Sink after conversion to salt.
        }


-- | Convert Core Discus to Core Salt.
discusToSalt
        :: A.Platform           -- ^ Platform configuation.
        -> A.Config             -- ^ Runtime config.
        -> [C.ModuleName]       -- ^ Names of modules transitively imported by the current one.
        -> C.Module () D.Name   -- ^ Core tetra module.
        -> ConfigDiscusToSalt   -- ^ Sinker config.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

discusToSalt platform runtimeConfig mnsInit mm config
 = do
        -- Expliciate the core module.
        --   This converts implicit function parameters and arguments to explicit ones
        --   as well as substituting in all type equations.
        mm_explicit
         <- B.coreSimplify
                BD.fragment (0 :: Int) C.expliciate
                mm

        liftIO $ B.pipeSink (renderIndent $ ppr mm_explicit)
                            (configSinkExplicit config)

        -- Perform the initialize transform.
        --   This wraps the main function with the default exception handler.
        let mm_initialize
                = DInitialize.initializeModule
                        runtimeConfig
                        mnsInit mm_explicit

        liftIO $ B.pipeSink (renderIndent $ ppr mm_initialize)
                            (configSinkInitialize config)

        -- Re-check the module before lambda lifting.
        --   The lambda lifter needs every node annotated with its type.
        mm_checked_lambdas
         <-  B.coreCheck
                "DiscusToSalt/lambdas" BD.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_initialize

        -- Perform lambda lifting.
        --   This hoists out nested lambda abstractions to top-level,
        --   producing supercombinators which are top-level functions.
        mm_lambdas
         <-  B.coreSimplify
                BD.fragment (0 :: Int) C.lambdas mm_checked_lambdas

        liftIO $ B.pipeSink (renderIndent $ ppr mm_lambdas)
                            (configSinkLambdas config)

        -- Re-check the module before performing unsharing.
        --   The unsharing transform needs every node annotated with its type.
        mm_checked_unshare
         <- B.coreCheck
                "DiscusToSalt/unshare" BD.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_lambdas

        -- Perform the unsharing transform.
        --   This adds extra unit parameters to all top-level bindings
        --   which are not functions, to make them functions.
        let mm_unshare  = CUnshare.unshareModule mm_checked_unshare

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_unshare)
                            (configSinkUnshare config)

        -- Perform the curry transform.
        --   This introduces special primops to handle partial application
        --   of our top-level supercombinators.
        mm_curry
         <- case DCurry.curryModule mm_unshare of
                Left err        -> throwE [B.ErrorDiscusConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_curry)
                            (configSinkCurry config)

        -- Prep before boxing transform.
        --   The boxing transform needs the program to be a-normalized.
        mm_prep_boxing
         <- B.coreSimplify BD.fragment (0 :: Int)
                (C.anormalize
                        (CNamify.makeNamifier (D.freshT "t"))
                        (CNamify.makeNamifier (D.freshX "x")))
                mm_curry

        -- Perform the boxing transform.
        --   This introduces explicitly unboxed values, using types (U# Nat#),
        --   and makes the original types like Nat# mean explicitly boxed objects.
        let mm_boxing
                = DBoxing.boxingModule mm_prep_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_boxing)
                            (configSinkBoxing config)

        -- Prep before conversion to salt.
        --   The to-salt conversion needs the program to be a-normalized.
        mm_prep_salt
         <- B.coreSimplify BD.fragment (0 :: Int)
                (  C.anormalize
                        (CNamify.makeNamifier (D.freshT "t"))
                        (CNamify.makeNamifier (D.freshX "x"))
                `mappend` C.flatten)
                mm_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_prep_salt)
                            (configSinkPrep config)

        -- Re-check before conversion to salt.
        --   The to-salt conversion needs every node annotated with its type.
        mm_checked_salt
         <- B.coreCheck
                "DiscusToSalt/toSalt" BD.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_prep_salt

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_salt)
                            (configSinkChecked config)

        -- Convert Core Tetra to Core Salt.
        mm_salt
         <- case D.saltOfDiscusModule
                platform
                runtimeConfig
                (C.profilePrimDataDefs D.profile)
                (C.profilePrimKinds    D.profile)
                (C.profilePrimTypes    D.profile) mm_checked_salt of
                Left err        -> throwE [B.ErrorDiscusConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_salt)
                            (configSinkSalt config)

        return mm_salt


