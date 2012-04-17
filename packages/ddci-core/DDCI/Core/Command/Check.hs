
module DDCI.Core.Command.Check
        ( cmdShowKind
        , cmdUniverse
        , cmdUniverse1
        , cmdUniverse2
        , cmdUniverse3
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
import DDC.Core.Lexer
import DDC.Type.Equiv
import DDC.Type.Transform.SpreadT
import DDC.Type.Universe
import qualified DDC.Type.Check         as T
import qualified DDC.Base.Parser        as BP


-- kind ------------------------------------------------------------------------
-- | Show the kind of a type.
cmdShowKind :: State -> Source -> String -> IO ()
cmdShowKind state source str
 | Language frag  <- stateLanguage state
 = let  toks    = fragmentLexExp frag source str
        eTK     = loadType (fragmentProfile frag) (nameOfSource source) toks
   in   case eTK of
         Left err       -> putStrLn $ renderIndent $ ppr err
         Right (t, k)   -> outDocLn state $ ppr t <+> text "::" <+> ppr k


-- universe -------------------------------------------------------------------
-- | Show the universe of some type.
cmdUniverse :: State -> Source -> String -> IO ()
cmdUniverse state source str
 | Language frag  <- stateLanguage state
 = do   result  <- cmdParseCheckType state source frag str
        case result of
         Just (t, _)
          | Just u      <- universeOfType t
          ->    outDocLn state $ ppr u

         _ ->   outDocLn state (text "no universe")


-- | Given the type of some thing (up one level)
--   show the universe of the thing.
cmdUniverse1 :: State -> Source -> String -> IO ()
cmdUniverse1 state source str
 | Language frag  <- stateLanguage state
 = do   result  <- cmdParseCheckType state source frag str
        case result of
         Just (t, _)
          | Just u      <- universeFromType1 t
          ->    outDocLn state $ ppr u

         _ ->   outDocLn state (text "no universe")


-- | Given the kind of some thing (up two levels)
--   show the universe of the thing.
cmdUniverse2 :: State -> Source -> String -> IO ()
cmdUniverse2 state source str
 | Language frag  <- stateLanguage state
 = do   result  <- cmdParseCheckType state source frag str
        case result of
         Just (t, _)
          | Just u      <- universeFromType2 t
          ->    outDocLn state $ ppr u

         _ ->   outDocLn state (text "no universe")


-- | Given the sort of some thing (up three levels)
--   show the universe of the thing.
--   We can't type check naked sorts, so just parse them.
cmdUniverse3 :: State -> Source -> String -> IO ()
cmdUniverse3 state source str
 | Language frag  <- stateLanguage state
 = let  profile = fragmentProfile frag
        kenv    = profilePrimKinds profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok (nameOfSource source) pType toks of
            Left err    -> outDocLn state $ ppr err
            Right t     -> goUniverse3 (spreadT kenv t)

        goUniverse3 tt
         = case universeFromType3 tt of
            Just u      -> outDocLn state $ ppr u
            Nothing     -> outDocLn state (text "no universe")

   in   goParse (fragmentLexExp frag source str)


-- | Parse a core type, and check its kind.
cmdParseCheckType 
        :: (Ord n, Show n, Pretty n)
        => State 
        -> Source 
        -> Fragment n err
        -> String 
        -> IO (Maybe (Type n, Kind n))

cmdParseCheckType _state source frag str
 = let  toks    = fragmentLexExp frag source str
        eTK     = loadType (fragmentProfile frag) (nameOfSource source) toks
   in   case eTK of
         Left err       
          -> do putStrLn $ renderIndent $ ppr err
                return Nothing

         Right (t, k)
          ->    return $ Just (t, k)


-- tequiv ---------------------------------------------------------------------
-- | Check if two types are equivlant.
cmdTypeEquiv :: State -> Source -> String -> IO ()
cmdTypeEquiv state source ss
 | Language frag <- stateLanguage state
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

        defs    = profilePrimDataDefs (fragmentProfile frag)
        kenv    = profilePrimKinds    (fragmentProfile frag)

        checkT t
         = case T.checkType defs kenv (spreadT kenv t) of
                Left err 
                 -> do  putStrLn $ renderIndent $ ppr err
                        return False

                Right{} 
                 ->     return True

   in goParse (fragmentLexExp frag source ss)



-- wtype ----------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: State -> Source -> String -> IO ()
cmdShowWType state source str
 | Language frag <- stateLanguage state
 = let  toks    = fragmentLexExp frag source str
        eTK     = loadWitness (fragmentProfile frag) (nameOfSource source) toks
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
 | Language frag <- stateLanguage state
 = cmdParseCheckExp state frag True source ss >>= goResult
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
 | Language fragment    <- stateLanguage state
 = cmdParseCheckExp state fragment True source ss >>= goResult
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
--
--   We include a flag to override the language profile to allow partially
--   applied primitives. Although a paticular evaluator (or backend) may not
--   support partially applied primitives, we want to accept them if we are
--   only loading an expression to check its type.
--
cmdParseCheckExp 
        :: (Ord n, Show n, Pretty n, Pretty (err ()))
        => State                -- ^ Interpreter state.
        -> Fragment n err       -- ^ The current language fragment.
        -> Bool                 -- ^ Allow partial application of primitives.
        -> Source               -- ^ Where this expression was sourced from.
        -> String               -- ^ Text to parse.
        -> IO (Maybe ( Exp () n
                     , Type n, Effect n, Closure n))

cmdParseCheckExp _state frag permitPartialPrims source str
 = goLoad (fragmentLexExp frag source str)
 where
        -- Override profile to allow partially applied primitives if we were
        -- told to do so.
        profile   = fragmentProfile frag
        features  = profileFeatures profile
        features' = features { featuresPartialPrims 
                             = featuresPartialPrims features || permitPartialPrims}
        profile'  = profile  { profileFeatures  = features' }
        frag'     = frag     { fragmentProfile  = profile'  }

        -- Parse and type check the expression.
        goLoad toks
         = case loadExp (fragmentProfile frag') (nameOfSource source) toks of
              Left err
               -> do    putStrLn $ renderIndent $ ppr err
                        return Nothing

              Right result
               -> goCheckFragment result

        -- Do fragment specific checks.
        goCheckFragment (x, t, e, c)
         = case fragmentCheckExp frag' x of
             Just err 
              -> do     putStrLn $ renderIndent $ ppr err
                        return Nothing

             Nothing  
              -> do     return (Just (x, t, e, c))


