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
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser
import DDC.Core.Parser.Tokens
import DDC.Core.Collect.FreeX
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Check         as T
import qualified DDC.Base.Parser        as BP
import qualified Data.Set               as Set

-- kind -------------------------------------------------------------------------------------------
cmdShowKind :: Int -> String -> IO ()
cmdShowKind lineStart ss
 = goParse (lexString lineStart ss)
 where
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goCheck t

        goCheck t
         = case T.checkType primKindEnv t of
                Left err        -> putStrLn $ pretty $ ppr err
                Right k         -> putStrLn $ pretty $ (ppr t <> text " :: " <> ppr k)



-- wtype ------------------------------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: Int -> String -> IO ()
cmdShowWType lineStart ss
 = cmdParseCheckWitness lineStart ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (w, t))
         = putStrLn $ pretty $ (ppr w <> text " :: " <> ppr t)


-- | Parse the given witness, and return it along with its type. 
--
--   If the expression had a parse error, undefined vars, or type error
--   then print this to the console.
cmdParseCheckWitness
        :: Int                  -- Starting line number for lexer.
        -> String 
        -> IO (Maybe (Witness Name, Type Name))

cmdParseCheckWitness lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pWitness toks of
                Left err 
                 -> do  putStrLn $ "parse error " ++ show err
                        return Nothing
                
                Right x  -> goCheck x

        -- Spread type annotations into binders,
        --   check for undefined variables, 
        --   and check its type.
        goCheck x
         = let  fvs     = freeX primTypeEnv x                   -- TODO: also check for free type vars
           in   if Set.null fvs
                 then   goResult x (checkWitness primKindEnv primTypeEnv x)
                 else do  
                        putStrLn $ pretty $ text "Undefined variables: " <> ppr fvs
                        return Nothing

        -- Expression had a type error.
        goResult _ (Left err)
         = do   putStrLn $ pretty $ ppr err
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
cmdShowType :: ShowTypeMode -> Int -> String -> IO ()
cmdShowType mode lineStart ss
 = cmdParseCheckExp lineStart ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (x, t, eff, clo))
         = case mode of
                ShowTypeAll
                 -> putStrLn $ pretty
                 $ vcat [ ppr x
                        , nest 4 $ text "::  " <> ppr t
                        , nest 4 $ text ":!: " <> ppr eff
                        , nest 4 $ text ":$: " <> ppr clo]
        
                ShowTypeValue
                 -> putStrLn $ pretty (ppr x <> text " :: " <> ppr t)
        
                ShowTypeEffect
                 -> putStrLn $ pretty (ppr x <> text " :! " <> ppr eff)

                ShowTypeClosure
                 -> putStrLn $ pretty (ppr x <> text " :$ " <> ppr clo)


-- | Check expression and reconstruct type annotations on binders.
cmdExpRecon   :: Int -> String -> IO ()
cmdExpRecon lineStart ss
 = cmdParseCheckExp lineStart ss >>= goResult
 where
        goResult Nothing
         = return ()

        goResult (Just (x, _, _, _))
         = putStrLn $ pretty (ppr x)


-- | Parse the given core expression, 
--   and return it, along with its type, effect and closure.
--
--   If the expression had a parse error, undefined vars, or type error
--   then print this to the console.
cmdParseCheckExp 
        :: Int          -- Starting line number.
        -> String 
        -> IO (Maybe ( Exp () Name
                     , Type Name, Effect Name, Closure Name))

cmdParseCheckExp lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pExp toks of
                Left err 
                 -> do  putStrLn $ pretty $ ppr err
                        return Nothing
                
                Right x  -> goCheck x

        -- Spread type annotations into binders,
        --   check for undefined variables, 
        --   and check its type.
        goCheck x
         = let  fvs     = freeX primTypeEnv x                        -- TODO also check for free type vars
           in   if Set.null fvs
                 then   goResult (checkExp primDataDefs primKindEnv primTypeEnv x)
                 else do  
                        putStrLn $ pretty $ text "Undefined variables: " <> ppr fvs
                        return Nothing

        -- Expression had a type error.
        goResult (Left err)
         = do   putStrLn $ pretty $ ppr err
                return  Nothing
         
        goResult (Right (x', t, eff, clo))
         =      return $ Just (x', t, eff, clo)

