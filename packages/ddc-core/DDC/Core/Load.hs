
-- | Parsing and type checking of core language constructs.
module DDC.Core.Load
        ( C.AnTEC (..)
        , Error (..)
        , loadModule
        , loadType
        , loadWitness
        , loadExp)
where
import DDC.Core.Transform.SpreadX
import DDC.Core.Fragment.Profile
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp
import DDC.Type.Transform.SpreadT
import DDC.Core.Module
import DDC.Base.Pretty
import DDC.Data.Token
import qualified DDC.Core.Fragment.Compliance   as I
import qualified DDC.Core.Parser                as C
import qualified DDC.Core.Check                 as C
import qualified DDC.Type.Check                 as T
import qualified DDC.Base.Parser                as BP


-- | Things that can go wrong when loading.
data Error n
        = ErrorParser     BP.ParseError
        | ErrorCheckType  (T.Error n)      
        | ErrorCheckExp   (C.Error () n)
        | ErrorCompliance (I.Error n)
        deriving Show


instance (Eq n, Show n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorParser     err'    
         -> vcat [ text "While parsing."
                 , indent 2 $ ppr err' ]

        ErrorCheckType  err'
         -> vcat [ text "When checking type."
                 , indent 2 $ ppr err' ]


        ErrorCheckExp   err'    
         -> vcat [ text "When checking expression."
                 , indent 2 $ ppr err' ]

        ErrorCompliance err'    
         -> vcat [ text "During fragment compliance check."
                 , indent 2 $ ppr err' ]


-- Module ---------------------------------------------------------------------
-- | Parse and type check a core module.
loadModule 
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n
        -> String 
        -> [Token (Tok n)] 
        -> Either (Error n) (Module (C.AnTEC () n) n)

loadModule profile sourceName toks'
 = goParse toks'
 where  defs    = profilePrimDataDefs profile
        kenv    = profilePrimKinds    profile
        tenv    = profilePrimTypes    profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pModule toks of
                Left err  -> Left (ErrorParser err)
                Right mm  -> goCheckType (spreadX kenv tenv mm)

        -- Check that the module is type sound.
        goCheckType mm
         = case C.checkModule defs kenv tenv mm of
                Left err  -> Left (ErrorCheckExp err)
                Right mm' -> goCheckCompliance mm'

        -- Check that the module compiles with the language fragment.
        goCheckCompliance mm
         = case I.complies profile mm of
                Just err  -> Left (ErrorCompliance err)
                Nothing   -> Right mm


-- Type -----------------------------------------------------------------------
-- | Parse and check a type
--   returning it along with its kind.
loadType
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n
        -> String 
        -> [Token (Tok n)] 
        -> Either (Error n) 
                  (Type n, Kind n)

loadType profile sourceName toks'
 = goParse toks'
 where  defs    = profilePrimDataDefs profile
        kenv    = profilePrimKinds    profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pType toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadT kenv t)

        -- Check the kind of the type.
        goCheckType t
         = case T.checkType defs kenv t of
                Left err  -> Left (ErrorCheckType err)
                Right k   -> Right (t, k)
        


-- Witness --------------------------------------------------------------------
-- | Parse and check a witness
--   returning it along with its type.
loadWitness
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n
        -> String 
        -> [Token (Tok n)] 
        -> Either (Error n) 
                  (Witness n, Type n)

loadWitness profile sourceName toks'
 = goParse toks'
 where  defs    = profilePrimDataDefs profile
        kenv    = profilePrimKinds    profile
        tenv    = profilePrimTypes    profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pWitness toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType w
         = case C.checkWitness defs kenv tenv w of
                Left err  -> Left (ErrorCheckExp err)
                Right k   -> Right (w, k)


-- Exp ------------------------------------------------------------------------
-- | Parse and check an expression
--   returning it along with its spec, effect and closure
loadExp
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n
        -> String 
        -> [Token (Tok n)] 
        -> Either (Error n) 
                  (Exp (C.AnTEC () n) n, Type n, Effect n, Closure n)   -- TODO: don't need to return TEC separately

loadExp profile sourceName toks'
 = goParse toks'
 where  defs    = profilePrimDataDefs profile
        kenv    = profilePrimKinds    profile
        tenv    = profilePrimTypes    profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok sourceName C.pExp toks of
                Left err  -> Left (ErrorParser err)
                Right t   -> goCheckType (spreadX kenv tenv t)

        -- Check the kind of the type.
        goCheckType x
         = case C.checkExp defs kenv tenv x of
                Left err            -> Left  (ErrorCheckExp err)
                Right (x', t, e, c) -> goCheckCompliance x' t e c

        -- Check that the module compiles with the language fragment.
        goCheckCompliance x t e c
         = case I.complies profile x of
                Just err  -> Left (ErrorCompliance err)
                Nothing   -> Right (x, t, e, c)

