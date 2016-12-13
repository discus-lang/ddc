
module DDC.Core.Machine.Process.Slurp
        ( slurpNetworks
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


data SlurpError
    = SlurpErrorNetworkBodyNotStreamProcessOrEnd (Exp () Name)
    | SlurpErrorNetworkNotPartiallyAppliedProcesNest (Exp () Name)
    | SlurpErrorNetworkEndNotUnit (Exp () Name)
    | SlurpErrorProcessBodyNotFunction (Exp () Name)
    deriving Show

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

slurpNetworkX :: (Bind Name, Exp () Name) -> Either SlurpError (Bind Name, Network) 
slurpNetworkX (networkBind,xx)
 | Just (_,body) <- takeXLamFlags xx
 -- TODO: currently assuming process# prim is unapplied
 , Just (NameOpMachine proc, [nestArg]) <- takeXFragApps body 
 , Just nest <- takeExpFromArg nestArg
 , OpProcess inLen _outLen <- proc
 , Just (inBinds,nestBody) <- takeXLams nest
 , length inBinds == inLen
 , Just inNames <- mapM takeNameOfBind inBinds
 = do
   (networkOuts,networkProcesses) <- slurpNetworkBody nestBody
   let networkIns = map Channel inNames
   return (networkBind, Network networkIns networkOuts networkProcesses)

 | otherwise
 = Left $ SlurpErrorNetworkNotPartiallyAppliedProcesNest xx

slurpNetworkBody :: Exp () Name -> Either SlurpError ([Channel], [Process]) 
slurpNetworkBody xx
 = case xx of
    XCase _ processX [AAlt (PData dacon streamOutBinds) xrest]
     | Just (NameDaConMachine (DaConTuple _)) <- takeNameOfDaCon dacon
     , Just (NameOpMachine (OpStream{}), processBodyArg : streamInArgs) <- takeXFragApps processX 
     , Just processBody <- takeExpFromArg processBodyArg
     , Just streamInNames  <- mapM (takeNameOfExp `compose` takeExpFromArg)  streamInArgs
     , Just streamOutNames <- mapM takeNameOfBind streamOutBinds
     -> do
        process <- slurpProcessBody processBody streamInNames streamOutNames
        (networkOuts,networkProcesses) <- slurpNetworkBody xrest
        return (networkOuts, process : networkProcesses)

    _
     | Just (dacon, streamOutArgs) <- takeXConApps xx
     , Just (NameDaConMachine (DaConTuple _)) <- takeNameOfDaCon dacon
     , Just streamOutNames <- mapM (takeNameOfExp `compose` takeExpFromArg) streamOutArgs
     -> do
        return (map Channel streamOutNames, [])

     | otherwise
     -> Left $ SlurpErrorNetworkBodyNotStreamProcessOrEnd xx
 where
  compose f g x
   = g x >>= f

slurpProcessBody :: Exp () Name -> [Name] -> [Name] -> Either SlurpError Process
slurpProcessBody xx ins outs
 | Just (sourceSinks,body) <- takeXLams xx
 = let _body' = substNames (zip sourceSinks (ins ++ outs)) body
   -- TODO
   in return $ Process (BlockNext (Label 0) []) Map.empty Map.empty
 | otherwise
 = Left $ SlurpErrorProcessBodyNotFunction xx
   

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

