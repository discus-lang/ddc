

module DDCI.Core.Command.Trans
        (cmdTrans)
where
import DDC.Core.Check
import DDC.Type.Equiv
import DDCI.Core.Transform
import DDCI.Core.State
import DDCI.Core.Command.Check
import DDCI.Core.Eval.Env
import DDC.Base.Pretty


-- | Apply the current transform to an expression.
cmdTrans :: State -> Int -> String -> IO ()
cmdTrans state lineStart str
 = cmdParseCheckExp lineStart str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore (Just (x, t1, eff1, clo1))
         = do   let x'  = applyTransformX (stateTransform state) x

                case checkExp primDataDefs primKindEnv primTypeEnv x' of
                  Right (_, t2, eff2, clo2)
                   |  equivT t1 t2
                   ,  equivT eff1 eff2
                   ,  equivT clo1 clo2
                   -> do putStrLn (pretty $ ppr x')
                         return ()

                   | otherwise
                   -> do putStrLn $ pretty $ vcat
                            [ text "* CRASH AND BURN: Transform is not type preserving."
                            , ppr x'
                            , text "::"  <+> ppr t2
                            , text ":!:" <+> ppr eff2
                            , text ":$:" <+> ppr clo2 ]

                  Left err
                   -> do putStrLn $ pretty $ vcat
                            [ text "* CRASH AND BURN: Type error in transformed program."
                            , ppr err
                            , text ""]

                         putStrLn "Transformed expression:"
                         
                         putStrLn (pretty $ ppr x')
                         return ()

