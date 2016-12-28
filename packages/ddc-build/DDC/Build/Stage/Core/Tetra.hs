
module DDC.Build.Stage.Core.Tetra
        (tetraToSalt)
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


---------------------------------------------------------------------------------------------------
-- | Convert Core Tetra to Core Salt.
tetraToSalt
        :: B.Sink               -- ^ Sink after making explicit.
        -> B.Sink               -- ^ Sink after lambda lifting.
        -> B.Sink               -- ^ Sink after unsharing.
        -> B.Sink               -- ^ Sink after the curry transform.
        -> B.Sink               -- ^ Sink after the boxing transform.
        -> B.Sink               -- ^ Sink after prep before to-salt conversion.
        -> B.Sink               -- ^ Sink after checking before to-salt conversion.
        -> B.Sink               -- ^ Sink after conversion to salt.
        -> A.Platform           -- ^ Platform configuation.
        -> A.Config             -- ^ Runtime config.
        -> C.Module () E.Name   -- ^ Core tetra module.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

tetraToSalt 
        sinkExplicit sinkLambdas sinkUnshare
        sinkCurry    sinkBoxing  
        sinkPrepSalt sinkCheckedSalt
        sinkSalt
        platform     runtimeConfig
        mm
 = do
        -- Expliciate the core module.
        mm_explicit     
         <- B.coreSimplify 
                BE.fragment (0 :: Int) C.expliciate
                mm

        liftIO $ B.pipeSink (renderIndent $ ppr mm_explicit) sinkExplicit


        -- Re-check the module before lambda lifting.
        mm_checked_lambdas
         <-  B.coreCheck    
                "TetraToSalt/lambdas" BE.fragment C.Recon 
                B.SinkDiscard B.SinkDiscard
                mm_explicit


        -- Perform lambda lifting.
        mm_lambdas
         <-  B.coreSimplify
                BE.fragment (0 :: Int) C.lambdas mm_checked_lambdas

        liftIO $ B.pipeSink (renderIndent $ ppr mm_lambdas) sinkLambdas


        -- Re-check the module before performing unsharing
        mm_checked_unshare
         <- B.coreCheck
                "TetraToSalt/unshare" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_lambdas


        -- Perform the unsharing transform.
        let mm_unshare  = CUnshare.unshareModule mm_checked_unshare

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_unshare) sinkUnshare


        -- Perform the curry transform.
        mm_curry    
         <- case ECurry.curryModule mm_unshare of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_curry) sinkCurry


        -- Prep before boxing transform.
        mm_prep_boxing
         <- B.coreSimplify BE.fragment (0 :: Int) 
                (C.anormalize
                        (CNamify.makeNamifier E.freshT)
                        (CNamify.makeNamifier E.freshX))
                mm_curry


        -- Perform the boxing transform.
        let mm_boxing
                = EBoxing.boxingModule mm_prep_boxing
        
        liftIO $ B.pipeSink (renderIndent $ ppr mm_boxing) sinkBoxing                


        -- Prep before conversion to salt.
        mm_prep_salt
         <- B.coreSimplify BE.fragment (0 :: Int)
                (  C.anormalize
                        (CNamify.makeNamifier E.freshT)
                        (CNamify.makeNamifier E.freshX)
                `mappend` C.flatten)
                mm_boxing

        liftIO $ B.pipeSink (renderIndent $ ppr mm_prep_salt) sinkPrepSalt


        -- Re-check before conversion to salt.
        mm_checked_salt
         <- B.coreCheck
                "TetraToSalt/toSalt" BE.fragment C.Recon
                B.SinkDiscard B.SinkDiscard
                mm_prep_salt

        liftIO $ B.pipeSink (renderIndent $ ppr mm_checked_salt) sinkCheckedSalt


        -- Convert core tetra to core salt.
        mm_salt
         <- case E.saltOfTetraModule platform runtimeConfig
                (C.profilePrimDataDefs E.profile)
                (C.profilePrimKinds    E.profile)
                (C.profilePrimTypes    E.profile) mm_checked_salt of
                Left err        -> throwE [B.ErrorTetraConvert err]
                Right mm'       -> return mm'

        liftIO $ B.pipeSink (renderIndent $ ppr mm_salt) sinkSalt


        return mm_salt


