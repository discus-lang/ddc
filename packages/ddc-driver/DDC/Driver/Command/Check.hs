module DDC.Driver.Command.Check
        ( cmdUniverse
        , cmdUniverse1
        , cmdUniverse2
        , cmdUniverse3
        , cmdShowKind
        , cmdTypeEquiv
        , cmdShowWType
        , cmdShowType
        , cmdExpRecon
        , ShowTypeMode(..)
        , cmdCheckModuleFromFile
        , cmdCheckModuleFromString
        , cmdParseCheckType
        , cmdParseCheckExp)
where
import DDC.Driver.Source
import DDC.Driver.Output
import DDC.Build.Language
import DDC.Core.Fragment
import DDC.Core.Load
import DDC.Core.Parser
import DDC.Core.Lexer
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Compounds
import DDC.Type.Transform.SpreadT
import DDC.Type.Universe
import DDC.Type.Equiv
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Parser        as BP
import qualified DDC.Type.Check         as T


-- universe -------------------------------------------------------------------
-- | Show the universe of some type.
cmdUniverse :: Language -> Source -> String -> IO ()
cmdUniverse language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , profile              <- fragmentProfile fragment
 = do   result          <- cmdParseCheckType source fragment str
        case result of
         Just (t, _)
          | Just u      <- universeOfType (profilePrimKinds profile) t
          ->    outDocLn $ ppr u

         _ ->   outDocLn $ text "no universe"


-- | Given the type of some thing (up one level)
--   show the universe of the thing.
cmdUniverse1 :: Language -> Source -> String -> IO ()
cmdUniverse1 language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , profile              <- fragmentProfile fragment
 = do   result          <- cmdParseCheckType source fragment str
        case result of
         Just (t, _)
          | Just u      <- universeFromType1 (profilePrimKinds profile) t
          ->    outDocLn $ ppr u

         _ ->   outDocLn $ text "no universe"


-- | Given the kind of some thing (up two levels)
--   show the universe of the thing.
cmdUniverse2 :: Language -> Source -> String -> IO ()
cmdUniverse2 language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 = do   result          <- cmdParseCheckType source fragment str
        case result of
         Just (t, _)
          | Just u      <- universeFromType2 t
          ->    outDocLn $ ppr u

         _ ->   outDocLn $ text "no universe"


-- | Given the sort of some thing (up three levels)
--   show the universe of the thing.
--   We can't type check naked sorts, so just parse them.
cmdUniverse3 :: Language -> Source -> String -> IO ()
cmdUniverse3 language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = let  srcName = nameOfSource source
        srcLine = lineStartOfSource source
        kenv    = profilePrimKinds profile

        -- Parse the tokens.
        goParse toks                
         = case BP.runTokenParser describeTok srcName 
                        (pType (contextOfProfile profile))
                        toks of
            Left err    -> outDocLn $ ppr err
            Right t     -> goUniverse3 (spreadT kenv t)

        goUniverse3 tt
         = case universeFromType3 tt of
            Just u      -> outDocLn $ ppr u
            Nothing     -> outDocLn $ text "no universe"

   in   goParse (fragmentLexExp fragment srcName srcLine str)


-- kind ------------------------------------------------------------------------
-- | Show the kind of a type.
cmdShowKind :: Language -> Source -> String -> IO ()
cmdShowKind language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = let  srcName = nameOfSource source
        srcLine = lineStartOfSource source
        toks    = fragmentLexExp fragment srcName srcLine str
        eTK     = loadType profile srcName toks
   in   case eTK of
         Left err       -> outDocLn $ ppr err
         Right (t, k)   -> outDocLn $ ppr t <+> text "::" <+> ppr k


-- tequiv ---------------------------------------------------------------------
-- | Check if two types are equivlant.
cmdTypeEquiv :: Language -> Source -> String -> IO ()
cmdTypeEquiv language source ss
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = let  srcName = nameOfSource source
        srcLine = lineStartOfSource source
        
        goParse toks
         = case BP.runTokenParser describeTok (nameOfSource source)
                        (do t1 <- pTypeAtom (contextOfProfile profile)
                            t2 <- pTypeAtom (contextOfProfile profile)
                            return (t1, t2))
                        toks
            of Left err -> outDocLn $ text "parse error " <> ppr err
               Right tt -> goEquiv tt
         
        goEquiv (t1, t2)
         = do   b1 <- checkT t1
                b2 <- checkT t2
                if b1 && b2 
                 then outStrLn $ show $ equivT t1 t2    
                 else return ()


        config  = T.configOfProfile profile
        kenv    = profilePrimKinds    profile

        checkT t
         = case T.checkType config kenv (spreadT kenv t) of
                Left err 
                 -> do  outDocLn $ ppr err
                        return False

                Right{} 
                 ->     return True

   in goParse (fragmentLexExp fragment srcName srcLine ss)


-- wtype ----------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: Language -> Source -> String -> IO ()
cmdShowWType language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , profile              <- fragmentProfile fragment
 = let  srcName = nameOfSource source
        srcLine = lineStartOfSource source
        toks    = fragmentLexExp fragment srcName srcLine str
        eTK     = loadWitness profile srcName toks
   in   case eTK of
         Left err       -> outDocLn $ ppr err
         Right (t, k)   -> outDocLn $ ppr t <+> text "::" <+> ppr k


-- check / type / effect / closure --------------------------------------------
-- | What components of the checked type to display.
data ShowTypeMode
        = ShowTypeAll
        | ShowTypeValue
        | ShowTypeEffect
        | ShowTypeClosure
        deriving (Eq, Show)


-- | Show the type of an expression.
cmdShowType :: Language -> ShowTypeMode -> Source -> String -> IO ()
cmdShowType language mode source ss
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , modules              <- bundleModules   bundle
 = cmdParseCheckExp fragment modules True source ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just x)
         = let  -- This will always succeed because a well typed expression
                -- is never a naked type or witness, and only those don't
                -- have annotations.
                Just annot      = takeAnnotOfExp x

                t               = annotType annot
                eff             = annotEffect annot
                clo             = annotClosure annot
           in case mode of
                ShowTypeAll
                 -> do  outDocLn $ ppr x
                        outDocLn $ text ":*:" <+> ppr t
                        outDocLn $ text ":!:" <+> ppr eff
                        outDocLn $ text ":$:" <+> ppr clo
        
                ShowTypeValue
                 ->     outDocLn $ ppr x <+> text "::" <+> ppr t
        
                ShowTypeEffect
                 ->     outDocLn $ ppr x <+> text ":!" <+> ppr eff

                ShowTypeClosure
                 ->     outDocLn $ ppr x <+> text ":$" <+> ppr clo


-- Recon ----------------------------------------------------------------------
-- | Check expression and reconstruct type annotations on binders.
cmdExpRecon :: Language -> Source -> String -> IO ()
cmdExpRecon language source ss
 | Language bundle      <- language
 , fragment             <- bundleFragment  bundle
 , modules              <- bundleModules   bundle
 =   cmdParseCheckExp fragment modules True source ss 
 >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just x)
         = outDocLn $ ppr x


-- Check ----------------------------------------------------------------------
-- | Parse and type-check a core module from a file.
cmdCheckModuleFromFile
        :: (Ord n, Show n, Pretty n, Pretty (err (AnTEC BP.SourcePos n)))
        => Fragment n err
        -> FilePath
        -> ErrorT String IO (Module (AnTEC BP.SourcePos n) n)

cmdCheckModuleFromFile fragment filePath
 = goLoad 
 where  lexModule =  fragmentLexModule fragment filePath 1

        -- Load and type-check the module.
        goLoad 
         = do   mModule <- liftIO 
                        $ loadModuleFromFile 
                                (fragmentProfile fragment) lexModule filePath
                case mModule of
                 Left  err      -> throwError (renderIndent $ ppr err)
                 Right mm       -> goCheckFragment mm

        -- Do fragment specific checks.
        goCheckFragment mm
         = case fragmentCheckModule fragment mm of
                Just err        -> throwError (renderIndent $ ppr err)
                Nothing         -> return mm


-- | Parse and type-check a core module from a string.
cmdCheckModuleFromString
        :: (Ord n, Show n, Pretty n, Pretty (err (AnTEC BP.SourcePos n)))
        => Fragment n err
        -> Source
        -> String
        -> ErrorT String IO (Module (AnTEC BP.SourcePos n) n)

cmdCheckModuleFromString fragment source str
 = goLoad
 where  lexModule = fragmentLexModule fragment 
                        (nameOfSource source) 
                        (lineStartOfSource source)

        -- Load and type-check the module.
        goLoad
         = let  mModule = loadModuleFromString
                                (fragmentProfile fragment) lexModule
                                (nameOfSource source)
                                str

            in  case mModule of
                  Left err       -> throwError (renderIndent $ ppr err)
                  Right mm       -> goCheckFragment mm

        goCheckFragment mm
         = case fragmentCheckModule fragment mm of
                Just err        -> throwError  (renderIndent $ ppr err)
                Nothing         -> return mm


-- | Parse a core type, and check its kind.
cmdParseCheckType 
        :: (Ord n, Show n, Pretty n)
        => Source
        -> Fragment n err
        -> String 
        -> IO (Maybe (Type n, Kind n))

cmdParseCheckType source frag str
 = let  srcName = nameOfSource source
        srcLine = lineStartOfSource source
        toks    = fragmentLexExp frag srcName srcLine str
        eTK     = loadType (fragmentProfile frag) srcName toks
   in   case eTK of
         Left err       
          -> do outDocLn $ ppr err
                return Nothing

         Right (t, k)
          ->    return $ Just (t, k)


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
        :: (Ord n, Show n, Pretty n, Pretty (err (AnTEC BP.SourcePos n)))
        => Fragment n err       -- ^ The current language fragment.
        -> ModuleMap (AnTEC () n) n -- ^ Current modules
        -> Bool                 -- ^ Allow partial application of primitives.
        -> Source               -- ^ Where this expression was sourced from.
        -> String               -- ^ Text to parse.
        -> IO (Maybe ( Exp (AnTEC BP.SourcePos n) n))

cmdParseCheckExp frag modules permitPartialPrims source str
 = goLoad (fragmentLexExp frag (nameOfSource source) (lineStartOfSource source) str)
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
         = case loadExp (fragmentProfile frag') modules (nameOfSource source) toks of
              Left err
               -> do    putStrLn $ renderIndent $ ppr err
                        return Nothing

              Right result
               -> goCheckFragment result

        -- Do fragment specific checks.
        goCheckFragment x
         = case fragmentCheckExp frag' x of
             Just err 
              -> do     putStrLn $ renderIndent $ ppr err
                        return Nothing

             Nothing  
              -> do     return (Just x)
