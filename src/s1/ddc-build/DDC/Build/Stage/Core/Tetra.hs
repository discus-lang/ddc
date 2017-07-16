
module DDC.Build.Stage.Core.Tetra
        ( tetraToShimmer
        , ConfigTetraToShimmer  (..)

        , tetraToSalt
        , ConfigTetraToSalt     (..))
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import DDC.Data.Pretty

import qualified DDC.Build.Stage.Core                   as B
import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Language.Tetra               as BE

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as CNamify
import qualified DDC.Core.Transform.Unshare             as CUnshare

import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Platform                 as A
import qualified DDC.Core.Salt.Runtime                  as A

import qualified DDC.Core.Tetra                         as E
import qualified DDC.Core.Tetra.Transform.Boxing        as EBoxing
import qualified DDC.Core.Tetra.Transform.Curry         as ECurry

import qualified DDC.Core.SMR                           as H


---------------------------------------------------------------------------------------------------
data ConfigTetraToShimmer
        = ConfigTetraToShimmer
        { configSinkShimmer     :: B.Sink       -- ^ Sink after conversion to shimmer.
        }


-- | Convert Core Tetra to Shimmer Code.
tetraToShimmer
        :: (Show a, Pretty a)
        => C.Module a E.Name                   -- ^ Core tetra module.
        -> ConfigTetraToShimmer                 -- ^ Sinker Config
        -> ExceptT [B.Error] IO (H.Module H.Name H.Name)

tetraToShimmer mm config
 = do
        mm_checked
         <-  B.coreCheck
                "TetraToShimmer/check" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm

        let mm_shimmer
                = case H.smrOfTetraModule
                        (C.profilePrimDataDefs E.profile)
                        (C.profilePrimKinds    E.profile)
                        (C.profilePrimTypes    E.profile)
                        mm_checked of
                    Left  err       -> error $ "tetraToShimmer fail" ++ err
                    Right mm'       -> mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_shimmer)
                            (configSinkShimmer config)

        return mm_shimmer


---------------------------------------------------------------------------------------------------
data ConfigTetraToSalt
        = ConfigTetraToSalt
        { configSinkExplicit    :: B.Sink       -- ^ Sink after making explicit.
        , configSinkLambdas     :: B.Sink       -- ^ Sink after lambda lifting.
        , configSinkUnshare     :: B.Sink       -- ^ Sink after unsharing.
        , configSinkCurry       :: B.Sink       -- ^ Sink after curry transform.
        , configSinkBoxing      :: B.Sink       -- ^ Sink after boxing transform.
        , configSinkPrep        :: B.Sink       -- ^ Sink after prep before to-salt conversion.
        , configSinkChecked     :: B.Sink       -- ^ Sink after checking before to-salt converion.
        , configSinkSalt        :: B.Sink       -- ^ Sink after conversion to salt.
        }


-- | Convert Core Tetra to Core Salt.
tetraToSalt
        :: A.Platform           -- ^ Platform configuation.
        -> A.Config             -- ^ Runtime config.
        -> C.Module () E.Name   -- ^ Core tetra module.
        -> ConfigTetraToSalt    -- ^ Sinker config.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

tetraToSalt platform runtimeConfig mm config
 = do
        -- Expliciate the core module.
        --   This converts implicit function parameters and arguments to explicit ones
        --   as well as substituting in all type equations.
        mm_explicit
         <- B.coreSimplify
                BE.fragment (0 :: Int) C.expliciate
                mm

        liftIO $ B.pipeSink (renderIndent $ ppr mm_explicit)
                            (configSinkExplicit config)


        -- Re-check the module before lambda lifting.
        --   The lambda lifter needs every node annotated with its type.
        mm_checked_lambdas
         <-  B.coreCheck
                "TetraToSalt/lambdas" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_explicit


        -- Perform lambda lifting.
        --   This hoists out nested lambda abstractions to top-level,
        --   producing supercombinators which are top-level functions.
        mm_lambdas
         <-  B.coreSimplify
                BE.fragment (0 :: Int) C.lambdas mm_checked_lambdas

        liftIO $ B.pipeSink (renderIndent $ ppr mm_lambdas)
                            (configSinkLambdas config)


        -- Re-check the module before performing unsharing.
        --   The unsharing transform needs every node annotated with its type.
        mm_checked_unshare
         <- B.coreCheck
                "TetraToSalt/unshare" BE.fragment C.Recon
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
         <- case ECurry.curryModule mm_unshare of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_curry)
                            (configSinkCurry config)


        -- Prep before boxing transform.
        --   The boxing transform needs the program to be a-normalized.
        mm_prep_boxing
         <- B.coreSimplify BE.fragment (0 :: Int)
                (C.anormalize
                        (CNamify.makeNamifier (E.freshT "t"))
                        (CNamify.makeNamifier (E.freshX "x")))
                mm_curry


        -- Perform the boxing transform.
        --   This introduces explicitly unboxed values, using types (U# Nat#),
        --   and makes the original types like Nat# mean explicitly boxed objects.
        let mm_boxing
                = EBoxing.boxingModule mm_prep_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_boxing)
                            (configSinkBoxing config)


        -- Prep before conversion to salt.
        --   The to-salt conversion needs the program to be a-normalized.
        mm_prep_salt
         <- B.coreSimplify BE.fragment (0 :: Int)
                (  C.anormalize
                        (CNamify.makeNamifier (E.freshT "t"))
                        (CNamify.makeNamifier (E.freshX "x"))
                `mappend` C.flatten)
                mm_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_prep_salt)
                            (configSinkPrep config)


        -- Re-check before conversion to salt.
        --   The to-salt conversion needs every node annotated with its type.
        mm_checked_salt
         <- B.coreCheck
                "TetraToSalt/toSalt" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_prep_salt

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_salt)
                            (configSinkChecked config)


        -- Convert Core Tetra to Core Salt.
        mm_salt
         <- case E.saltOfTetraModule platform runtimeConfig
                (C.profilePrimDataDefs E.profile)
                (C.profilePrimKinds    E.profile)
                (C.profilePrimTypes    E.profile) mm_checked_salt of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_salt)
                            (configSinkSalt config)

        return mm_salt


