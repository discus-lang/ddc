
module DDC.Main.Command.Load
        (cmdLoad)
where
import 

cmdLoad :: Bundle -> String -> FilePath -> IO ()


case bundleFromFilePath config filePath of
             Nothing 
              -> error "nope"

             Just (Bundle frag modules _ _ rules) 
              | Fragment _ _ _ _ _ _ mkNamT mkNamX zero <- frag
              ->  case parseSimplifier 
                        (SimplifierDetails
                             mkNamT mkNamX 
                                (Map.assocs rules) 
        
                                -- Collect all definitions from modules
                                (I.lookupTemplateFromModules
                                        $ Map.elems modules)

                        -- Module-specific templates
                        (map (\(n,m) -> (n, I.lookupTemplateFromModules [m])) 
                                        $ Map.assocs modules))
                (concat $ intersperse " " rest) of

         Just simpl
          -> do chatStrLn state "ok"
                return $ state { stateBundle = Bundle frag modules zero simpl rules }

         Nothing
          -> do putStrLn "transform spec parse error"
                return state


          do  let Just bundle = bundleFromFilePath config filePath
                str        <- readFile filePath
                cmdLoad bundle
                        (SourceFile filePath)
                        str

