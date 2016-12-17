
module DDC.Core.Machine.Process.Slurp
        ( slurpNetworks
        , SlurpError    (..)
        )
where
import DDC.Core.Machine.Prim
import DDC.Core.Machine.Process.Base
import DDC.Type.Exp
import DDC.Core.Exp.Annot
import DDC.Core.Module
import qualified DDC.Core.Transform.SubstituteXX as Subst

import Control.Monad (when)
import qualified Data.Map as Map

import DDC.Core.Pretty          ()
import DDC.Data.Pretty (Doc, Pretty(..), ppr, text, line, (<>))


data SlurpError
    = SlurpErrorNetworkBodyNotStreamProcessOrEnd (Exp () Name)
    | SlurpErrorNetworkNotPartiallyAppliedProcesNest (Exp () Name)
    | SlurpErrorNetworkEndNotUnit (Exp () Name)
    | SlurpErrorProcessBodyNotFunction (Exp () Name)
    | SlurpErrorOther Doc (Exp () Name)
    deriving Show

instance Pretty SlurpError where
 ppr (SlurpErrorNetworkBodyNotStreamProcessOrEnd x)
  = text "Network body: expected a stream# or return tuple. Got:" <> line <> ppr x
 ppr (SlurpErrorNetworkNotPartiallyAppliedProcesNest x)
  = text "Network body: expected a top-level process# with single argument of process nest. Got:" <> line <> ppr x
 ppr (SlurpErrorNetworkEndNotUnit x)
  = text "Network body: end of module should be unit. Got:" <> line <> ppr x
 ppr (SlurpErrorProcessBodyNotFunction x)
  = text "Process body: expected lambda with sources and sinks. Got:" <> line <> ppr x
 ppr (SlurpErrorOther other x)
  = other <> line <> ppr x


-- | Slurp process networks from the top level of a module.
slurpNetworks :: Module () Name -> Either SlurpError [(Bind Name, Network)]
slurpNetworks mm
 = slurpNetworksX $ moduleBody mm

slurpNetworksX :: Exp () Name -> Either SlurpError [(Bind Name, Network)]
slurpNetworksX xx
 = do   let (binds, xx') = takeLetsAll xx
        when (xx' /= xUnit ()) $ Left $ SlurpErrorNetworkEndNotUnit xx'
        networks' <- mapM slurpNetworkX binds
        return networks'

just :: l -> Maybe r -> Either l r
just l Nothing = Left l
just _ (Just r) = Right r

slurpNetworkX :: (Bind Name, Exp () Name) -> Either SlurpError (Bind Name, Network) 
slurpNetworkX (networkBind,xx)
 = do   let err str = just $ SlurpErrorOther (text str) xx
        (NameOpMachine proc, nestArgs) <- err "takeXFragApps" $ takeXFragApps xx
        -- TODO: currently assuming process# prim is unapplied
        nest <- case takeExpsFromArgs nestArgs of
            [nest] -> return nest
            args   -> Left (SlurpErrorOther (text "takeExpsFromArgs: " <> ppr args) xx)

        (inLen,_outLen) <- case proc of
            OpProcess inLen outLen -> return (inLen,outLen)
            _ -> Left (SlurpErrorOther (text "expected prim process_I_O#, got: " <> ppr proc) xx)

        (inBinds,nestBody) <- err "takeXLams" $ takeXLams nest
        when (length inBinds /= inLen) $ Left $ SlurpErrorOther (text "length inBinds") xx
        inNames <- err "takeNameOfBind" $ mapM takeNameOfBind inBinds
        (networkOuts,networkProcesses) <- slurpNetworkBody nestBody
        let networkIns = map Channel inNames
        return (networkBind, Network networkIns networkOuts networkProcesses)

slurpNetworkBody :: Exp () Name -> Either SlurpError ([Channel], [Process]) 
slurpNetworkBody xx
 = case xx of
    XCase _ processX [AAlt (PData dacon streamOutBinds) xrest]
     | Just (NameDaConMachine (DaConTuple _)) <- takeNameOfDaCon dacon
     , Just (NameOpMachine (OpStream{}), streamArgs) <- takeXFragApps processX 
     , processBody : streamInExps <- takeExpsFromArgs streamArgs
     , Just streamInNames  <- mapM takeNameOfExp  streamInExps
     , Just streamOutNames <- mapM takeNameOfBind streamOutBinds
     -> do
        process <- slurpProcessBody processBody streamInNames streamOutNames
        (networkOuts,networkProcesses) <- slurpNetworkBody xrest
        return (networkOuts, process : networkProcesses)

    _
     | Just (dacon, streamOutArgs) <- takeXConApps xx
     , Just (NameDaConMachine (DaConTuple _)) <- takeNameOfDaCon dacon
     , Just streamOutNames <- mapM takeNameOfExp $ takeExpsFromArgs streamOutArgs
     -> do
        return (map Channel streamOutNames, [])

     | otherwise
     -> Left $ SlurpErrorNetworkBodyNotStreamProcessOrEnd xx

slurpProcessBody :: Exp () Name -> [Name] -> [Name] -> Either SlurpError Process
slurpProcessBody xx ins outs
 | Just (sourceSinks,body) <- takeXLams xx
 = do   let chans   = Map.fromList
                    $ map (\(k,v) -> (Channel k, v))
                    $ (map (,ChannelInput) ins ++ map (,ChannelOutput) outs)
        let body'   = substNames (zip sourceSinks (ins ++ outs)) body
        let (ls,l0) = takeLetsAll body'
        l0' <- takeBlockNext l0

        let (lbs,lxs) = unzip ls
        let mkLbl x = Label <$> takeNameOfBind x
        let erLbl = SlurpErrorOther (text "Get label of process blocks: " <> ppr lbs) xx
        labels <- just erLbl $ mapM mkLbl lbs
        blocks <- mapM takeBlockInfo lxs
        return $ Process l0' (Map.fromList $ zip labels blocks)  chans
 | otherwise
 = Left $ SlurpErrorProcessBodyNotFunction xx


takeBlockNext :: Exp () Name -> Either SlurpError BlockNext
takeBlockNext xx
 | Just (l,args) <- takeXApps xx
 , Just l'       <- takeNameOfExp l
 , args'         <- takeExpsFromArgs args
 = return $ BlockNext (Label l') args'
 | Just l'       <- takeNameOfExp xx
 = return $ BlockNext (Label l') []
 | otherwise
 = Left $ SlurpErrorOther (text "takeBlockNext") xx

takeBlockInfo :: Exp () Name -> Either SlurpError BlockInfo
takeBlockInfo xx
 = let (binds,block) = takeXLams' xx
   in BlockInfo binds <$> takeBlock block
 where
  takeXLams' xxx
   = case takeXLams xxx of
     Nothing  -> ([],xxx)
     Just ret -> ret

takeBlock :: Exp () Name -> Either SlurpError Block
takeBlock block
 = do   (prim,args) <- just (SlurpErrorOther (text "takeBlockInfo takeXFragApps" <> ppr block) block) $ takeXFragApps block
        case prim of
         NameOpMachine OpPull
          | [chan, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          -> BlockPull (Channel chan') <$> takeBlockNext next
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlockInfo pull bad arguments: " <> ppr args) block

         NameOpMachine OpPush
          | [chan, val, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          -> BlockPush (Channel chan') val <$> takeBlockNext next
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlockInfo push bad arguments: " <> ppr args) block

         NameOpMachine OpDrop
          | [chan, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          -> BlockDrop (Channel chan') <$> takeBlockNext next
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlockInfo drop bad arguments: " <> ppr args) block

            

         NameVar{}      -> BlockJump <$> takeBlockNext block
         NameVarMod{}   -> BlockJump <$> takeBlockNext block

         _ -> Left $ SlurpErrorOther (text "takeBlockInfo non-block primitive: " <> ppr prim) block
            


takeLetsAll :: Exp () Name -> ([(Bind Name, Exp () Name)], Exp () Name)
takeLetsAll xx
 = case xx of
    XLet _ lets xrest
      -> let (binds', xrest') = takeLetsAll xrest
         in case lets of
             LRec binds -> (binds ++ binds', xrest')
             LLet b x   -> ((b,x) :  binds', xrest')
             LPrivate{} ->          (binds', xrest')
    _ -> ([], xx)

substNames :: [(Bind Name, Name)] -> Exp () Name -> Exp () Name
substNames bs xx
 = Subst.substituteXXs (map (\(a,b) -> (a, XVar () (UName b))) bs) xx

