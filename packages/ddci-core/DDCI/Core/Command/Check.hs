
module DDCI.Core.Command.Check
        ( cmdShowKind
        , cmdTypeEquiv
        , cmdShowWType
        , cmdShowType
        , cmdExpRecon
        , ShowTypeMode(..)
        , cmdParseCheckExp)
where
import DDCI.Core.Mode
import DDCI.Core.Language
import DDCI.Core.State
import DDCI.Core.Output
import DDC.Core.Language.Profile
import DDC.Core.Load
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Parser
import DDC.Core.Parser.Tokens
import DDC.Type.Equiv
import DDC.Type.Transform.SpreadT
import qualified DDC.Type.Check         as T
import qualified DDC.Base.Parser        as BP


-- kind ------------------------------------------------------------------------
-- | Show the kind of a type.
cmdShowKind :: State -> Source -> String -> IO ()
cmdShowKind state source str
 | Language profile  <- stateLanguage state
 = let  toks    = fragmentLex source str
        eTK     = loadType profile (nameOfSource source) toks
   in   case eTK of
         Left err       -> putStrLn $ renderIndent $ ppr err
         Right (t, k)   -> outDocLn state $ ppr t <+> text "::" <+> ppr k


-- tequiv ---------------------------------------------------------------------
-- | Check if two types are equivlant.
cmdTypeEquiv :: State -> Source -> String -> IO ()
cmdTypeEquiv state source ss
 | Language profile  <- stateLanguage state
 = let
        goParse toks
         = case BP.runTokenParser describeTok (nameOfSource source)
                        (do t1 <- pTypeAtom
                            t2 <- pTypeAtom
                            return (t1, t2))
                        toks
            of Left err -> putStrLn $ renderIndent $ text "parse error " <> ppr err
               Right tt -> goEquiv tt
         
        goEquiv (t1, t2)
         = do   b1 <- checkT t1
                b2 <- checkT t2
                if b1 && b2 
                 then putStrLn $ show $ equivT t1 t2    
                 else return ()

        defs    = profilePrimDataDefs profile
        kenv    = profilePrimKinds    profile

        checkT t
         = case T.checkType defs kenv (spreadT kenv t) of
                Left err 
                 -> do  putStrLn $ renderIndent $ ppr err
                        return False

                Right{} 
                 ->     return True

   in goParse (fragmentLex source ss)



-- wtype ----------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: State -> Source -> String -> IO ()
cmdShowWType state source str
 | Language profile  <- stateLanguage state
 = let  toks    = fragmentLex source str
        eTK     = loadWitness profile (nameOfSource source) toks
   in   case eTK of
         Left err       -> putStrLn $ renderIndent $ ppr err
         Right (t, k)   -> outDocLn state $ ppr t <+> text "::" <+> ppr k


-- check / type / effect / closure --------------------------------------------
-- | What components of the checked type to display.
data ShowTypeMode
        = ShowTypeAll
        | ShowTypeValue
        | ShowTypeEffect
        | ShowTypeClosure
        deriving (Eq, Show)


-- | Show the type of an expression.
cmdShowType :: State -> ShowTypeMode -> Source -> String -> IO ()
cmdShowType state mode source ss
 | Language profile <- stateLanguage state
 = cmdParseCheckExp state profile source ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (x, t, eff, clo))
         = case mode of
                ShowTypeAll
                 -> do  outDocLn state $ ppr x
                        outDocLn state $ text ":*: " <+> ppr t
                        outDocLn state $ text ":!:" <+> ppr eff
                        outDocLn state $ text ":$:" <+> ppr clo
        
                ShowTypeValue
                 -> outDocLn state $ ppr x <+> text "::" <+> ppr t
        
                ShowTypeEffect
                 -> outDocLn state $ ppr x <+> text ":!" <+> ppr eff

                ShowTypeClosure
                 -> outDocLn state $ ppr x <+> text ":$" <+> ppr clo


-- Recon ----------------------------------------------------------------------
-- | Check expression and reconstruct type annotations on binders.
cmdExpRecon :: State -> Source -> String -> IO ()
cmdExpRecon state source ss
 | Language profile <- stateLanguage state
 = cmdParseCheckExp state profile source ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (x, _, _, _))
         = outDocLn state $ ppr x


-- Check ----------------------------------------------------------------------
-- | Parse the given core expression, 
--   and return it, along with its type, effect and closure.
--
--   If the expression had a parse error, undefined vars, or type error
--   then print this to the console.
cmdParseCheckExp 
        :: ( Fragment n err
           , Ord n, Show n, Pretty n
           , Pretty err)
        => State
        -> Profile n
        -> Source
        -> String 
        -> IO (Maybe ( Exp () n
                     , Type n, Effect n, Closure n))

cmdParseCheckExp _state profile source str
 = goLoad (fragmentLex source str)
 where
        -- Parse and type check the expression.
        goLoad toks
         = case loadExp profile (nameOfSource source) toks of
              Left err
               -> do    putStrLn $ renderIndent $ ppr err
                        return Nothing

              Right result
               -> goCheckFragment result

        -- Do fragment specific checks.
        goCheckFragment (x, t, e, c)
         = case fragmentCheckExp x of
             Just err 
              -> do     putStrLn $ renderIndent $ ppr err
                        return Nothing

             Nothing  
              -> do     return (Just (x, t, e, c))
