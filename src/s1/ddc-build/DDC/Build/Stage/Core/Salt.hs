
module DDC.Build.Stage.Core.Salt
        ( saltCompileViaLlvm
        , saltToLlvm)
where
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import qualified System.Directory                       as System
import qualified System.FilePath                        as FilePath

import DDC.Data.Pretty

import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Builder                      as B
import qualified DDC.Build.Stage.Core                   as BC
import qualified DDC.Build.Language.Salt                as BA

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Codec.Text.Pretty             as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as CNamify
import qualified DDC.Core.Transform.Reannotate          as CReannotate

import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Platform                 as A
import qualified DDC.Core.Salt.Transform.Transfer       as ATransfer
import qualified DDC.Core.Salt.Transform.Slotify        as ASlotify
import qualified DDC.Core.Llvm.Convert                  as ALlvm

import qualified DDC.Llvm.Syntax                        as L
import qualified DDC.Llvm.Pretty                        as L

---------------------------------------------------------------------------------------------------
-- | Compile Salt Code using the system Llvm compiler.
saltCompileViaLlvm
        :: (Show a, Pretty a)
        => String               -- ^ Name of source module, for error messages.
        -> B.Builder            -- ^ Builder for target platform.
        -> FilePath             -- ^ Path for object file.
        -> Maybe  FilePath      -- ^ Path for executable file.
        -> Maybe [FilePath]     -- ^ Paths of other object files to link with.
        -> Bool                 -- ^ Whether to add stack slot code.
        -> Bool                 -- ^ Whether to keep intermediate Llvm files.
        -> Bool                 -- ^ Whether to keep intermediate Asm files.
        -> B.Sink               -- ^ Sink after prep simplification.
        -> B.Sink               -- ^ Sink after introducing stack slots.
        -> B.Sink               -- ^ Sink after transfer transform.
        -> C.Module a A.Name    -- ^ Core Salt module.
        -> ExceptT [B.Error] IO ()

saltCompileViaLlvm
        srcName builder pathO mPathExe mPathsOther
        bSlotify bKeepLlvmFiles bKeepAsmFiles
        sinkPrep sinkSlots sinkTransfer
        mm

 = do
        -- Decide where to place the build products.
        let pathLL      = FilePath.replaceExtension pathO ".ddc.ll"
        let pathS       = FilePath.replaceExtension pathO ".ddc.s"

        -- Convert Salt code to Llvm code.
        let platform    = B.buildSpec builder
        mm_llvm
         <- saltToLlvm
                srcName platform bSlotify
                sinkPrep sinkSlots sinkTransfer
                mm

        -- Render Llvm module as a string.
        let llConfig    = L.configOfVersion $ Just $ B.buildLlvmVersion builder
        let llMode      = L.prettyModeModuleOfConfig llConfig
        let strLlvm     = renderIndent $ pprModePrec llMode (0 :: Int) mm_llvm

        -- Write out Llvm source file.
        liftIO $ writeFile pathLL strLlvm

        -- Compile Llvm source file into .s file.
        liftIO $ B.buildLlc builder pathLL pathS

        -- Assemble .s file into .o file.
        liftIO $ B.buildAs  builder pathS  pathO

        -- Link .o file into an executable if we were asked for one.
        let pathsO = pathO : (concat $ maybeToList mPathsOther)
        (case mPathExe of
          Nothing       -> return ()
          Just pathExe  -> liftIO $ B.buildLdExe builder pathsO pathExe)

        -- Remove intermediate .ll files if we weren't asked for them.
        when (not bKeepLlvmFiles)
         $ liftIO $ System.removeFile pathLL

        -- Remove intermediate .asm files if we weren't asked for them.
        when (not bKeepAsmFiles)
         $ liftIO $ System.removeFile pathS

        return ()


---------------------------------------------------------------------------------------------------
-- | Convert Salt code to Shadow Stack Slotted LLVM.
saltToLlvm
        :: (Show a, Pretty a)
        => String               -- ^ Name of source module, for error messages.
        -> A.Platform           -- ^ Platform to produce code for.
        -> Bool                 -- ^ Whether to introduce stack slots.
        -> B.Sink               -- ^ Sink after prep simplification.
        -> B.Sink               -- ^ Sink after introducing stack slots.
        -> B.Sink               -- ^ Sink after transfer transform.
        -> C.Module a A.Name    -- ^ Core Salt module.
        -> ExceptT [B.Error] IO L.Module

saltToLlvm
        _srcName platform bAddSlots
        sinkPrep sinkSlots sinkTransfer
        mm
 = do
        -- Pretty print a-normalized salt modules with a separate column
        -- for the binders, as the code is mostly a list of function calls
        -- and primop applications.
        let md_flat :: PrettyMode (C.Module a A.Name)
            md_flat
                = (pprDefaultMode :: PrettyMode (C.Module a A.Name))
                { C.modeModuleLets = pprDefaultMode
                                   { C.modeLetsColumnTypes  = True }}

        let pprModule mm' = pprModePrec md_flat 0 mm'

        -- Normalize code in preparation for conversion.
        mm_simpl
         <- BC.coreSimplify
                BA.fragment (0 :: Int)
                (C.anormalize (CNamify.makeNamifier A.freshT)
                              (CNamify.makeNamifier A.freshX))
                mm

        liftIO $ B.pipeSink (renderIndent $ pprModule mm_simpl) sinkPrep


        -- Check normalized code to produce type annotations on every node.
        mm_checked
         <- BC.coreCheck
                "saltToLlvm" BA.fragment Nothing C.Recon
                B.SinkDiscard B.SinkDiscard mm_simpl

        liftIO $ B.pipeSink (renderIndent $ pprModule mm_simpl) sinkPrep


        -- Insert shadow stack slot management instructions,
        --  if we were asked for them.
        mm_slotify
         <- if bAddSlots
             then do mm' <- case ASlotify.slotifyModule () mm_checked of
                                Left err   -> throwE [B.ErrorSaltConvert "saltToLlvm/slotify" err]
                                Right mm'' -> return mm''

                     liftIO $ B.pipeSink (renderIndent $ pprModule mm') sinkSlots
                     return mm'

             else return mm_checked


        -- Insert control transfer primops.
        mm_transfer
         <- case ATransfer.transferModule mm_slotify of
                Left err   -> throwE [B.ErrorSaltConvert "saltToLlvm/transfer" err]
                Right mm'  -> return mm'

        liftIO $ B.pipeSink (renderIndent $ pprModule mm_transfer) sinkTransfer


        -- Convert to LLVM source code.
        srcLlvm
         <- case ALlvm.convertModule platform
                  (CReannotate.reannotate (const ()) mm_transfer) of
                Left  err -> throwE [B.ErrorSaltConvert "saltToLlvm/convert" err]
                Right mm' -> return mm'

        return srcLlvm

