
module DDC.Code.Config
        (locateBaseLibrary)
where

-- | Locate the path holding the code for the runtime system
--   and base library.
locateBaseLibrary :: IO FilePath
locateBaseLibrary 
        = return "packages/ddc-code"
