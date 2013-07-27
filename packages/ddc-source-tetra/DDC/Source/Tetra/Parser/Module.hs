
module DDC.Source.Tetra.Parser.Module
        ( -- * Modules
          pModule
        , pTypeSig
        , pImportKindSpec
        , pImportTypeSpec

          -- * Top-level things
        , pTop)
where
import DDC.Source.Tetra.Parser.Exp
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Base.Pretty
import qualified DDC.Base.Parser        as P


-- Module ---------------------------------------------------------------------
-- | Parse a core module.
pModule :: (Ord n, Pretty n) 
        => Context
        -> Parser n (Module P.SourcePos n)
pModule c
 = do   _sp     <- pTokSP KModule
        name    <- pModuleName

        -- -- exports { SIG;+ }
        -- tExports 
        -- <- P.choice
        --    [do pTok KExports
        --        pTok KBraceBra
        --        sigs    <- P.sepEndBy1 (pTypeSig c) (pTok KSemiColon)
        --        pTok KBraceKet
        --        return sigs

        --    ,   return []]

        -- -- imports { SIG;+ }
        -- tImportKindsTypes
        -- <- P.choice
        --    [do pTok KImports
        --        pTok KBraceBra
        --        importKinds     <- P.sepEndBy (pImportKindSpec c) (pTok KSemiColon)
        --        importTypes     <- P.sepEndBy (pImportTypeSpec c) (pTok KSemiColon)
        --        pTok KBraceKet
        --        return (importKinds, importTypes)

        --    ,   return ([], [])]

        --let (tImportKinds, tImportTypes)
        --        = tImportKindsTypes

        pTok KWhere
        pTok KBraceBra

        -- TOP;+
        tops    <- P.sepEndBy (pTop c) (pTok KSemiColon)

        pTok KBraceKet

        -- ISSUE #295: Check for duplicate exported names in module parser.
        --  The names are added to a unique map, so later ones with the same
        --  name will replace earlier ones.
        return  $ Module
                { moduleName            = name
                , moduleExportedTypes   = []
                , moduleExportedValues  = []
                , moduleImportedModules = []
                , moduleTops            = tops }


-- | Parse a type signature.
pTypeSig 
        :: Ord n 
        => Context -> Parser n (n, Type n)        

pTypeSig c
 = do   var     <- pVar
        pTok KColonColon
        t       <- pType c
        return  (var, t)


-- | Parse the type signature of an imported variable.
pImportKindSpec 
        :: (Ord n, Pretty n) 
        => Context -> Parser n (n, (QualName n, Kind n))

pImportKindSpec c
 =   pTok KType
 >>  P.choice
 [      -- Import with an explicit external name.
        -- Module.varExternal with varLocal
   do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        k       <- pType c
        return  (n, (qn, k))

 , do   n       <- pName
        pTok KColonColon
        k       <- pType c
        return  (n, (QualName (ModuleName []) n, k))
 ]        


-- | Parse the type signature of an imported variable.
pImportTypeSpec 
        :: (Ord n, Pretty n) 
        => Context -> Parser n (n, (QualName n, Type n))

pImportTypeSpec c
 = P.choice
 [      -- Import with an explicit external name.
        -- Module.varExternal with varLocal
   do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        t       <- pType c
        return  (n, (qn, t))

 , do   n       <- pName
        pTok KColonColon
        t       <- pType c
        return  (n, (QualName (ModuleName []) n, t))
 ]        


-- Top Level -----------------------------------------------------------------
pTop    :: Ord n 
        => Context -> Parser n (Top P.SourcePos n)
pTop c
 = P.choice
 [ do   -- A top-level, possibly recursive binding.
        (b, x)          <- pLetBinding c
        let Just sp     = takeAnnotOfExp x
        return  $ TopBind sp b x
 ]

