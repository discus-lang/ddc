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
        ( seaOfSaltModule
        , initRuntime
        , seaNameOfSuper
        , seaNameOfLocal
        , sanitizeName
        , Error (..))

where
import DDC.Core.Salt.Convert.Type
import DDC.Core.Salt.Convert.Init
import DDC.Core.Salt.Convert.Name
import DDC.Core.Salt.Convert.Super
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
                | not withPrelude       = empty
                | otherwise
                = vcat    
                $  [ text "// Includes for helper macros and the runtime system. -----------------"
                   , text "#include \"Runtime.h\""
                   , text "#include \"Primitive.h\"" 
                   , line ]


        -- Globals for the runtime system -------
        --   If this is the main module then we define the globals for the
        --   runtime system at top-level.
        let cGlobals
                | not withPrelude       = empty

                | isMainModule mm
                = vcat  
                $  [ text "// Definitions of the runtime system variables. -----------------------"
                   , text "addr_t _DDC__heapTop = 0;"
                   , text "addr_t _DDC__heapMax = 0;" 
                   , line ]

                | otherwise
                = vcat  
                $  [ text "// External definitions for the runtime system variables. -------------"
                   , text "extern addr_t _DDC__heapTop;"
                   , text "extern addr_t _DDC__heapMax;" 
                   , line ]


        -- Import external symbols --------------
        dsImport
         <- mapM (\(misrc, nSuper, tSuper)
                        -> convSuperTypeM Env.empty misrc Nothing nSuper tSuper)
                 [ (Just isrc, nSuper, tSuper)
                        | (nSuper, isrc) <- C.moduleImportValues mm
                        , let tSuper     =  typeOfImportSource isrc ]

        let cExterns
                | not withPrelude       = empty
                | otherwise             
                = vcat  
                $  [ text "// External definitions for imported symbols. -------------------------"]
                ++ [ text "extern " <> doc <> semi | doc <- dsImport ]
                ++ [ line ]
                

        -- Function prototypes ------------------
        --   These are for the supers defined in this module, so that they
        --   can be recursive, and the function definitions don't need to
        --   be emitted in a particular order.
                
        dsProto         
         <- mapM (\(mesrc, nSuper, tSuper) 
                        -> convSuperTypeM Env.empty Nothing mesrc nSuper tSuper)
                 [ (mesrc, nSuper, tSuper) 
                        | (BName nSuper tSuper, _) <- bxs
                        , let mesrc     = lookup nSuper (moduleExportValues mm) ]
                                         
        let cProtos
                | not withPrelude       = empty
                | otherwise
                = vcat 
                $  [ text "// Function prototypes for locally defined supers. --------------------"]
                ++ [ doc <> semi | doc <- dsProto ]
                ++ [ line ]


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
        let convSuperM' (BName n t) x
                = convSuperM pp mm kenv tenv n t x

            convSuperM' _ x
                = throw $ ErrorFunctionInvalid x

        dsSupers <- mapM (uncurry convSuperM') bxs
        let cSupers     
                = vcat  
                $  [ text "// Code for locally defined supers. -----------------------------------"]
                ++ dsSupers

        -- Paste everything together ------------
        return  $  cIncludes    -- Includes for helper macros and the runtime system.
                <> cGlobals     -- Definitions of the runtime system variables.
                <> cExterns     -- External definitions for imported symbols.
                <> cProtos      -- Function prototypes for locally defined supers.
                <> cSupers      -- Code for locally defined supers.

 | otherwise
 = throw $ ErrorNoTopLevelLetrec mm


