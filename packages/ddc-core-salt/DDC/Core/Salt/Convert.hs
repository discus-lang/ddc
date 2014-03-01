-- | Convert the Disciple Core Salt into to real C code.
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      a-normalised,
--      have a control-transfer primop at the end of every function body
--        (these are added by DDC.Core.Salt.Convert.Transfer)
--      
module DDC.Core.Salt.Convert
        ( Error (..)
        , seaOfSaltModule)
where
import DDC.Core.Salt.Convert.Super
import DDC.Core.Salt.Convert.Type
import DDC.Core.Salt.Convert.Base
import DDC.Core.Salt.Name
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Core.Module                          as C
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Control.Monad.Check                  (throw, evalCheck)
import qualified DDC.Type.Env                   as Env


-- | Convert a Disciple Core Salt module to C-source text.
seaOfSaltModule
        :: Show a 
        => Bool                 -- ^ Whether to emit top-level include macros.
                                --   Emitting makes the code easier to read during testing.
        -> Platform             -- ^ Target platform specification
        -> Module a Name        -- ^ Module to convert.
        -> Either (Error a) Doc

seaOfSaltModule withPrelude pp mm
 = {-# SCC seaOfSaltModule #-}
   evalCheck () $ convModuleM withPrelude pp mm


-- | Convert a Salt module to C source text.
convModuleM :: Show a => Bool -> Platform -> Module a Name -> ConvertM a Doc
convModuleM withPrelude pp mm@(ModuleCore{})
 | ([LRec bxs], _) <- splitXLets $ moduleBody mm
 = do   
        -- Top-level includes ---------
        let cIncludes
                | not withPrelude
                = []

                | otherwise
                = [ text "#include \"Runtime.h\""
                  , text "#include \"Primitive.h\""
                  , empty ]

        -- Import external symbols ----
        let nts =  map snd $ C.moduleImportValues mm
        docs    <- mapM (uncurry $ convFunctionTypeM Env.empty) 
                        [(src, typeOfImportSource src) | src <- nts]
        let cExterns
                | not withPrelude = []
                | otherwise       = [ text "extern " <> doc <> semi | doc <- docs ]

        -- RTS def --------------------
        -- If this is the main module then we need to declare
        -- the global RTS state.
        let cGlobals
                | not withPrelude
                = []

                | isMainModule mm
                = [ text "addr_t _DDC_Runtime_heapTop = 0;"
                  , text "addr_t _DDC_Runtime_heapMax = 0;"
                  , empty ]

                | otherwise
                = [ text "extern addr_t _DDC_Runtime_heapTop;"
                  , text "extern addr_t _DDC_Runtime_heapMax;"
                  , empty ]

        -- Function prototypes --------
        let srcsProto
                = [ImportSourceSea (renderPlain (ppr n)) t | (BName n t, _) <- bxs]
        dsProto <- mapM (uncurry $ convFunctionTypeM Env.empty)
                        [(src, typeOfImportSource src) | src <- srcsProto]
        let cProtos
                | not withPrelude = []
                | otherwise       = [ doc <> semi | doc <- dsProto ]

        -- Super-combinator definitions.
        let kenv = Env.fromList [ BName n (typeOfImportSource isrc) 
                                | (n, isrc) <- moduleImportTypes mm ]

        let tenv = Env.fromList [ BName n (typeOfImportSource isrc)
                                | (n, isrc) <- moduleImportValues mm ]

        cSupers <- mapM (uncurry (convSuperM pp kenv tenv)) bxs

        -- Paste everything together
        return  $  vcat 
                $  cIncludes 
                ++ cExterns
                ++ cGlobals
                ++ cProtos
                ++ cSupers

 | otherwise
 = throw $ ErrorNoTopLevelLetrec mm


