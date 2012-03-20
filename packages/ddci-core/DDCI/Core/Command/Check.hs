module DDCI.Core.Command.Check
        ( cmdShowKind
        , cmdTypeEquiv
        , cmdShowWType
        , cmdShowType
        , cmdExpRecon
        , ShowTypeMode(..)
        , cmdParseCheckExp)
where
import DDCI.Core.Fragment
import DDCI.Core.State
import DDCI.Core.IO
import DDC.Core.Language.Profile
import DDC.Core.Load
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Parser.Tokens
import DDC.Type.Equiv
import DDC.Type.Transform.SpreadT
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Check         as T
import qualified DDC.Base.Parser        as BP


-- kind ------------------------------------------------------------------------
-- | Show the kind of a type.
cmdShowKind :: State -> Int -> String -> IO ()
cmdShowKind state lineStart str
 | StateProfile profile  <- stateProfile state
 = let  toks    = fragmentLex lineStart str
        eTK     = loadType profile "<interactive>" toks
   in   case eTK of
         Left err       -> putStrLn $ renderIndent $ ppr err
         Right (t, k)   -> outDocLn state $ ppr t <+> text "::" <+> ppr k


-- tequiv ---------------------------------------------------------------------
-- | Check if two types are equivlant.
cmdTypeEquiv :: State -> Int -> String -> IO ()
cmdTypeEquiv state lineStart ss
 | StateProfile profile  <- stateProfile state
 = let
        goParse toks
         = case BP.runTokenParser describeTok "<interactive>"
                        (do t1 <- T.pTypeAtom
                            t2 <- T.pTypeAtom
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

   in goParse (fragmentLex lineStart ss)



-- wtype ----------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: State -> Int -> String -> IO ()
cmdShowWType state lineStart str
 | StateProfile profile  <- stateProfile state
 = let  toks    = fragmentLex lineStart str
        eTK     = loadWitness profile "<interactive>" toks
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
cmdShowType :: State -> ShowTypeMode -> Int -> String -> IO ()
cmdShowType state mode lineStart ss
 | StateProfile profile <- stateProfile state
 = cmdParseCheckExp state profile lineStart ss >>= goResult
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
cmdExpRecon :: State -> Int -> String -> IO ()
cmdExpRecon state lineStart ss
 | StateProfile profile <- stateProfile state
 = cmdParseCheckExp state profile lineStart ss >>= goResult
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
        -> Int          -- Starting line number.
        -> String 
        -> IO (Maybe ( Exp () n
                     , Type n, Effect n, Closure n))

cmdParseCheckExp _state profile lineStart str
 = goLoad (fragmentLex lineStart str)
 where
        -- Parse and type check the expression.
        goLoad toks
         = case loadExp profile "<interactive>" toks of
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
