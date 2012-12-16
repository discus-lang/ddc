{-# LANGUAGE CPP #-}
-- | This module is responsible for finding out where the runtime system
--   and base library code is installed.
--
module DDC.Code.Config
        (locateBaseLibrary)
where

-------------------------------------------------------------------------------
-- When the compiler has been installed via 'cabal' install then we don't
-- have the full development source tree. 
--
-- In this case the ddc-code.cabal file defines the preprocessesor flag
-- DDC_CABAL_INSTALLED, which tells us we can import the Cabal generated
-- Paths_ddc_code module and ask it where its put our files.
-- 
#if defined(DDC_CABAL_INSTALLED)
import System.FilePath
import Paths_ddc_code           as Cabal

locateBaseLibrary :: IO FilePath
locateBaseLibrary
 = do   -- The rest of the files are in the same directory as the LICENSE
        -- file, so we can just ask for that one and take the directory name.
        licenseName     <- Cabal.getDataFileName "LICENSE"
        let basePath    = takeDirectory licenseName
        return basePath

-------------------------------------------------------------------------------
-- When the compiler is built from the development source tree via make
-- this code is in "packages/ddc-code", and we need to run 'ddc' from the 
-- root of the source tree so it can find this path.
#else
locateBaseLibrary :: IO FilePath
locateBaseLibrary 
        = return "packages/ddc-code"
#endif

