
module DDC.Core.Parser.Module
        (pModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Parser.Type
import DDC.Core.Parser.Exp
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Compounds
import DDC.Base.Pretty
import qualified DDC.Base.Parser        as P
import qualified Data.Map               as Map


-- Module ---------------------------------------------------------------------
-- | Parse a core module.
pModule :: (Ord n, Pretty n) 
        => Parser n (Module () n)
pModule 
 = do   pTok KModule
        name    <- pModuleName

        -- exports { SIG;+ }
        tExports 
         <- P.choice
            [do pTok KExports
                pTok KBraceBra
                sigs    <- P.sepEndBy1 pTypeSig (pTok KSemiColon)
                pTok KBraceKet
                return sigs

            ,   return []]

        -- imports { SIG;+ }
        tImportKindsTypes
         <- P.choice
            [do pTok KImports
                pTok KBraceBra
                importKinds     <- P.sepEndBy pImportKindSpec (pTok KSemiColon)
                importTypes     <- P.sepEndBy pImportTypeSpec (pTok KSemiColon)
                pTok KBraceKet
                return (importKinds, importTypes)

            ,   return ([], [])]

        let (tImportKinds, tImportTypes)
                = tImportKindsTypes

        pTok KWith

        -- LET;+
        lts     <- P.sepBy1 pLets (pTok KIn)

        -- The body of the module consists of the top-level bindings wrapped
        -- around a unit constructor place-holder.
        let body = makeXLets () lts (xUnit ())

        -- TODO: make having duplicate names in the imports 
        --       or exports list a parse error, so we never build a bad map. 
        return  $ ModuleCore
                { moduleName            = name
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.fromList tExports
                , moduleImportKinds     = Map.fromList tImportKinds
                , moduleImportTypes     = Map.fromList tImportTypes
                , moduleBody            = body }


-- | Parse a type signature.
pTypeSig :: Ord n => Parser n (n, Type n)        
pTypeSig
 = do   var     <- pVar
        pTok KColonColon
        t       <- pType
        return  (var, t)


-- | Parse the type signature of an imported variable.
pImportKindSpec 
        :: (Ord n, Pretty n) 
        => Parser n (n, (QualName n, Kind n))

pImportKindSpec 
 =   pTok KType
 >>  P.choice
 [      -- Import with an explicit external name.
        -- Module.varExternal with varLocal
   do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        k       <- pType
        return  (n, (qn, k))

 , do   n       <- pName
        pTok KColonColon
        k       <- pType
        return  (n, (QualName (ModuleName []) n, k))
 ]        


-- | Parse the type signature of an imported variable.
pImportTypeSpec 
        :: (Ord n, Pretty n) 
        => Parser n (n, (QualName n, Type n))

pImportTypeSpec 
 = P.choice
 [      -- Import with an explicit external name.
        -- Module.varExternal with varLocal
   do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        t       <- pType
        return  (n, (qn, t))

 , do   n       <- pName
        pTok KColonColon
        t       <- pType
        return  (n, (QualName (ModuleName []) n, t))
 ]        
