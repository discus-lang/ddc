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


-- | Convert a Disciple Core Salt module to C source text.
convModuleM :: Show a => Bool -> Platform -> Module a Name -> ConvertM a Doc
convModuleM withPrelude pp mm@(ModuleCore{})
 | ([LRec bxs], _) <- splitXLets $ moduleBody mm
 = do   
        -- Top-level includes -------------------
        -- These include runtime system functions and macro definitions that
        -- the generated code refers to directly.
        let cIncludes
                | not withPrelude       = []
                | otherwise
                = [ text "#include \"Runtime.h\""
                  , text "#include \"Primitive.h\""
                  , empty ]


        -- Import external symbols --------------
        let nts         =  map snd $ C.moduleImportValues mm
        docs            <- mapM (uncurry $ convFunctionTypeM Env.empty) 
                                [(src, typeOfImportSource src) | src <- nts]
        let cExterns
                | not withPrelude       = []
                | otherwise             = [ text "extern " <> doc <> semi | doc <- docs ]


        -- Globals for the Runtime system -------
        --   If this is the main module then we define the globals for the
        --   runtime system at top-level.
        let cGlobals
                | not withPrelude       = []

                | isMainModule mm
                = [ text "addr_t _DDC_Runtime_heapTop = 0;"
                  , text "addr_t _DDC_Runtime_heapMax = 0;"
                  , empty ]

                | otherwise
                = [ text "extern addr_t _DDC_Runtime_heapTop;"
                  , text "extern addr_t _DDC_Runtime_heapMax;"
                  , empty ]


        -- Function prototypes ------------------
        --   These are for the supers defined in this module, so that they
        --   can be recursive, and the function definitions don't need to
        --   be emitted in a particular order.
        
        -- We reuse the same code that generates imports for external
        -- symbols, so construct some intermediate 'ImportSourceSea' to re-import
        -- our locally defined functions.
        let srcsProto   = [ImportSourceSea (renderPlain (ppr n)) t 
                                | (BName n t, _) <- bxs]
        
        dsProto         <- mapM (uncurry $ convFunctionTypeM Env.empty)
                                [(src, typeOfImportSource src) | src <- srcsProto]
        let cProtos
                | not withPrelude       = []
                | otherwise             = [ doc <> semi | doc <- dsProto ]


        -- Super-combinator definitions ---------
        --   This is the code for locally defined functions.
        
        -- Build the top-level kind environment.
        let kenv        = Env.fromList 
                        $ [ BName n (typeOfImportSource isrc) 
                                | (n, isrc) <- moduleImportTypes mm ]

        -- Build the top-level type environment.
        let tenv        = Env.fromList 
                        $ [ BName n (typeOfImportSource isrc)
                                | (n, isrc) <- moduleImportValues mm ]

        -- Convert all the super definitions to C code.
        cSupers <- mapM (uncurry (convSuperM pp kenv tenv)) bxs


        -- Paste everything together ------------
        return  $  vcat 
                $  cIncludes    -- Includes for the Runtime and helper macros.
                ++ cExterns     -- Externs for imported symbols.
                ++ cGlobals     -- Globals for the runtime system.
                ++ cProtos      -- Function prototypes for locally defined supers.
                ++ cSupers      -- C code for locally defined supers.

 | otherwise
 = throw $ ErrorNoTopLevelLetrec mm


