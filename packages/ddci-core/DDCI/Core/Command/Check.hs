module DDCI.Core.Command.Check
        ( cmdShowKind
        , cmdShowWType
        , cmdShowType
        , cmdExpRecon
        , ShowTypeMode(..)
        , cmdParseCheckExp)
where
import DDCI.Core.Eval.Env
import DDCI.Core.Eval.Name
import DDCI.Core.State
import DDCI.Core.IO
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser
import DDC.Core.Parser.Tokens
import DDC.Core.Collect.FreeX
import DDC.Core.Transform.SpreadX
import DDC.Type.Transform.SpreadT
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Check         as T
import qualified DDC.Base.Parser        as BP
import qualified Data.Set               as Set


-- kind -------------------------------------------------------------------------------------------
cmdShowKind :: State -> Int -> String -> IO ()
cmdShowKind state lineStart ss
 = goParse (lexString lineStart ss)
 where
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" T.pType toks of 
                Left err        -> outDocLn state $ text "parse error " <> ppr err
                Right t         -> goCheck t

        goCheck t
         = case T.checkType primKindEnv (spreadT primKindEnv t) of
                Left err        -> putStrLn $ renderIndent $ ppr err
                Right k         -> outDocLn state $ ppr t <+> text "::" <+> ppr k


-- wtype ------------------------------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: State -> Int -> String -> IO ()
cmdShowWType state lineStart ss
 = cmdParseCheckWitness state lineStart ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (w, t))
         = outDocLn state $ ppr w <> text " :: " <> ppr t


-- | Parse the given witness, and return it along with its type. 
--
--   If the expression had a parse error, undefined vars, or type error
--   then print this to the console.
cmdParseCheckWitness
        :: State
        -> Int                  -- Starting line number for lexer.
        -> String 
        -> IO (Maybe (Witness Name, Type Name))

cmdParseCheckWitness state lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pWitness toks of
                Left err 
                 -> do  outDocLn state $ text "parse error " <> ppr err
                        return Nothing
                
                Right x  -> goCheck x

        -- Spread type annotations into binders,
        --   check for undefined variables, 
        --   and check its type.
        goCheck x
         = let  fvs     = freeX primTypeEnv x                   -- TODO: also check for free type vars
                x'      = spreadX primKindEnv primTypeEnv x
           in   if Set.null fvs
                 then   goResult x' (checkWitness primKindEnv primTypeEnv x')
                 else do  
                        outDocLn state $ text "Undefined variables: " <> ppr fvs
                        return Nothing

        -- Expression had a type error.
        goResult _ (Left err)
         = do   putStrLn $ renderIndent $ ppr err
                return  Nothing
         
        goResult x (Right t)
         =      return $ Just (x, t)



-- check / type / effect / closure ----------------------------------------------------------------
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
 = cmdParseCheckExp state lineStart ss >>= goResult
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


-- | Check expression and reconstruct type annotations on binders.
cmdExpRecon :: State -> Int -> String -> IO ()
cmdExpRecon state lineStart ss
 = cmdParseCheckExp state lineStart ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (x, _, _, _))
         = outDocLn state $ ppr x


-- | Parse the given core expression, 
--   and return it, along with its type, effect and closure.
--
--   If the expression had a parse error, undefined vars, or type error
--   then print this to the console.
cmdParseCheckExp 
        :: State
        -> Int          -- Starting line number.
        -> String 
        -> IO (Maybe ( Exp () Name
                     , Type Name, Effect Name, Closure Name))

cmdParseCheckExp state lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pExp toks of
                Left err 
                 -> do  putStrLn $ renderIndent $ ppr err
                        return Nothing
                
                Right x  -> goCheck x

        -- Spread type annotations into binders,
        --   check for undefined variables, 
        --   and check its type.
        goCheck x
         = let  fvs     = freeX   primTypeEnv x                        -- TODO also check for free type vars
                x'      = spreadX primKindEnv primTypeEnv x
           in   if Set.null fvs
                 then   goResult (checkExp primDataDefs primKindEnv primTypeEnv x')
                 else do  
                        outDocLn state $ text "Undefined variables: " <> ppr fvs
                        return Nothing

        -- Expression had a type error.
        goResult (Left err)
         = do   putStrLn $ renderIndent $ ppr err
                return  Nothing
         
        goResult (Right (x', t, eff, clo))
         =      return $ Just (x', t, eff, clo)

