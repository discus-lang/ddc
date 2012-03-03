
-- | Parsing and type checking of core language constructs.
module DDC.Core.Load
        ( loadModule
        , loadType
        , loadWitness)
where
import DDC.Core.Transform.SpreadX
import DDC.Core.Language.Profile
import DDC.Core.Parser.Tokens
import DDC.Core.Exp
import DDC.Type.Transform.SpreadT
import DDC.Core.Module
import DDC.Base.Lexer
import DDC.Base.Pretty
import qualified DDC.Core.Parser        as C
import qualified DDC.Core.Check         as C
import qualified DDC.Type.Check         as T
import qualified DDC.Type.Parser        as T
import qualified DDC.Base.Parser        as BP


-- | Things that can go wrong when loading.
data Error n
        = ErrorParser     BP.ParseError
        | ErrorCheckType  (T.Error n)      
        | ErrorCheckExp   (C.Error () n)
        deriving Show


instance (Eq n, Show n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorParser     err'  -> ppr err'
        ErrorCheckType  err'  -> ppr err'
        ErrorCheckExp   err'  -> ppr err'


-- Module ---------------------------------------------------------------------
-- | Parse and type check a core module.
loadModule 
        :: (Eq n, Ord n, Show n, Pretty n)
        => Profile n
        -> String 
        -> [Token (Tok n)] 
        -> Either (Error n) (Module () n)

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

        -- Check the type of the expression.
        goCheckType mm
         = case C.checkModule defs kenv tenv mm of
                Left err  -> Left (ErrorCheckExp err)
                Right mm' -> Right mm'


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
         = case BP.runTokenParser describeTok sourceName T.pType toks of
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
