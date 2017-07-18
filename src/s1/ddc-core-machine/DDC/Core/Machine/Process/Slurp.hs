
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
        -- NOTE: currently assuming process# prim is unapplied
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
        let (lbs,lxs) = unzip ls
        let mkLbl x = Label <$> takeNameOfBind x
        let erLbl = SlurpErrorOther (text "Get label of process blocks: " <> ppr lbs) xx
        labels <- just erLbl $ mapM mkLbl lbs
        args   <- mapM takeLabelArgs lxs
        let largs = Map.fromList $ zip labels args

        blocks <- mapM (takeBlock largs . snd . takeXLams') lxs
        l0' <- takeBlockNext largs l0
        return $ Process l0' (Map.fromList $ zip labels blocks)  chans
 | otherwise
 = Left $ SlurpErrorProcessBodyNotFunction xx

takeLabelArgs :: Exp () Name -> Either SlurpError [Variable]
takeLabelArgs xx
 = do   let mkVar b = Variable <$> takeNameOfBind b
        let (lams,_) = takeXLams' xx
        let err = SlurpErrorOther (text "Can't get label arguments") xx
        just err $ mapM mkVar lams

takeBlockNext :: Map.Map Label [Variable] -> Exp () Name -> Either SlurpError BlockNext
takeBlockNext largs xx
 | Just (l,args) <- takeXApps xx
 , Just l'       <- takeNameOfExp l
 , args'         <- takeExpsFromArgs args
 , Just vars     <- Map.lookup (Label l') largs
 -- NOTE: we still need to check args and vars are same length.
 -- Except for pull the args must actually be one shorter than vars because of the extra on the end
 = return $ BlockNext (Label l') (Map.fromList $ zip vars args')
 | Just l'       <- takeNameOfExp xx
 = return $ BlockNext (Label l') Map.empty
 | otherwise
 = Left $ SlurpErrorOther (text "takeBlockNext") xx

takeBlock :: Map.Map Label [Variable] -> Exp () Name -> Either SlurpError Block
takeBlock largs block
 = do   (prim,args) <- just (SlurpErrorOther (text "takeBlock takeXNameApps " <> ppr block) block) $ takeXNameApps block
        case prim of
         NameOpMachine OpPull
          | [chan, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          , Right next' <- takeBlockNext largs next
          , Just pullargs <- Map.lookup (bnLabel next') largs
          , pullvar : _   <- reverse pullargs
          -> return $ BlockPull (Channel chan') pullvar next'
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlock pull bad arguments: " <> ppr args) block

         NameOpMachine OpPush
          | [chan, val, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          -> BlockPush (Channel chan') val <$> takeBlockNext largs next
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlock push bad arguments: " <> ppr args) block

         NameOpMachine OpDrop
          | [chan, next] <- takeExpsFromArgs args
          , Just chan' <- takeNameOfExp chan
          -> BlockDrop (Channel chan') <$> takeBlockNext largs next
          | otherwise
          -> Left $ SlurpErrorOther (text "takeBlock drop bad arguments: " <> ppr args) block



         NameVar{}      -> BlockJump <$> takeBlockNext largs block
         NameVarMod{}   -> BlockJump <$> takeBlockNext largs block

         _ -> Left $ SlurpErrorOther (text "takeBlock non-block primitive: " <> ppr prim) block



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

takeXLams' :: Exp () Name -> ([Bind Name], Exp () Name)
takeXLams' xx
 = case takeXLams xx of
   Nothing  -> ([],xx)
   Just ret -> ret

takeXNameApps :: Exp a n -> Maybe (n, [Arg a n])
takeXNameApps xx
 = do (f,as)    <- takeXApps xx
      n         <- takeNameOfExp f
      return (n, as)


substNames :: [(Bind Name, Name)] -> Exp () Name -> Exp () Name
substNames bs xx
 = Subst.substituteXXs (map (\(a,b) -> (a, XVar () (UName b))) bs) xx

