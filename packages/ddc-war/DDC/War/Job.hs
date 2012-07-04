{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -fno-warn-orphans #-}
module DDC.War.Job where
import DDC.War.Driver.Base
import qualified DDC.War.Job.CompileDC  as CompileDC
import qualified DDC.War.Job.CompileDS  as CompileDS
import qualified DDC.War.Job.CompileHS  as CompileHS
import qualified DDC.War.Job.Diff       as Diff
import qualified DDC.War.Job.RunDCX     as RunDCX
import qualified DDC.War.Job.RunExe     as RunExe
import qualified DDC.War.Job.Shell      as Shell
import BuildBox.Pretty


instance Spec CompileDC.Spec CompileDC.Result where
 specActionName _               = "compile"
 buildFromSpec                  = CompileDC.build
 productOfResult _ result       
        = ProductStatus (ppr result) (CompileDC.resultSuccess result)


instance Spec CompileDS.Spec  CompileDS.Result where
 specActionName _               = "compile"
 buildFromSpec                  = CompileDS.build
 productOfResult _ result       
        = ProductStatus (ppr result) (CompileDS.resultSuccess result)


instance Spec CompileHS.Spec  CompileHS.Result where
 specActionName _               = "compile"
 buildFromSpec                  = CompileHS.build
 productOfResult _ result
        = ProductStatus (ppr result) (CompileHS.resultSuccess result)


instance Spec Diff.Spec       Diff.Result where
 specActionName _               = "diff"
 buildFromSpec                  = Diff.build
 productOfResult _ result
  = case result of
        Diff.ResultSame                 
         -> ProductStatus (ppr result) True

        Diff.ResultDiff ref out diff    
         -> ProductDiff ref out diff


instance Spec RunDCX.Spec     RunDCX.Result where
 specActionName _               = "run"
 buildFromSpec                  = RunDCX.build
 productOfResult _ result
        = ProductStatus (ppr result) (RunDCX.resultSuccess result)


instance Spec RunExe.Spec     RunExe.Result where
 specActionName _               = "run"
 buildFromSpec                  = RunExe.build
 productOfResult _ result
        = ProductStatus (ppr result) (RunExe.resultSuccess result)


instance Spec Shell.Spec      Shell.Result where
 specActionName _               = "shell"
 buildFromSpec                  = Shell.build
 productOfResult _ result
        = ProductStatus (ppr result) (Shell.resultSuccess result)




