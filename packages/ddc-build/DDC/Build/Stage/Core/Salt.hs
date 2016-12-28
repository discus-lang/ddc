
module DDC.Build.Stage.Core.Salt
        ( saltToSea )
where
import Control.Monad.Trans.Except

import DDC.Data.Pretty

import qualified DDC.Build.Pipeline.Error               as B
import qualified DDC.Build.Pipeline.Sink                as B
import qualified DDC.Build.Stage.Core                   as BC
import qualified DDC.Build.Language.Salt                as BA

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as CNamify

import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Platform                 as A
import qualified DDC.Core.Salt.Transfer                 as ATransfer


-- | Convert Salt code to sea.
saltToSea
        :: (Show a, Pretty a)
        => String               -- ^ Name of source module, for error messages.
        -> A.Platform           -- ^ Platform to produce code for.
        -> C.Module a A.Name    -- ^ Core Salt module.
        -> ExceptT [B.Error] IO String

saltToSea srcName platform mm
 = do
        mm_simpl
         <- BC.coreSimplify
                BA.fragment (0 :: Int) 
                (C.anormalize (CNamify.makeNamifier A.freshT)
                              (CNamify.makeNamifier A.freshX))
                mm

        mm_checked
         <- BC.coreCheck
                srcName BA.fragment C.Recon
                B.SinkDiscard B.SinkDiscard mm_simpl

        mm_transfer
         <- case ATransfer.transferModule mm_checked of
                Left err        -> throwE [B.ErrorSaltConvert err]
                Right mm'       -> return mm'

        case A.seaOfSaltModule True platform mm_transfer of
         Left  err -> throwE [B.ErrorSaltConvert err]
         Right str -> return (renderIndent str)



