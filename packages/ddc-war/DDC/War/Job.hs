{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -fno-warn-orphans #-}
module DDC.War.Job where
import DDC.War.Driver.Base
import qualified DDC.War.Job.CompileDCE as CompileDCE
import qualified DDC.War.Job.CompileDS  as CompileDS
import qualified DDC.War.Job.CompileHS  as CompileHS
import qualified DDC.War.Job.Diff       as Diff
import qualified DDC.War.Job.RunDCX     as RunDCX
import qualified DDC.War.Job.RunExe     as RunExe
import qualified DDC.War.Job.Shell      as Shell
import BuildBox.Pretty


instance Spec CompileDCE.Spec CompileDCE.Result where
 jobActionName _                = "compile"
 buildFromSpec                  = CompileDCE.build
 productOfResult _ result       = ProductStatus (ppr result) 


instance Spec CompileDS.Spec  CompileDS.Result where
 jobActionName _                = "compile"
 buildFromSpec                  = CompileDS.build
 productOfResult _ result       = ProductStatus (ppr result)


instance Spec CompileHS.Spec  CompileHS.Result where
 jobActionName _                = "compile"
 buildFromSpec                  = CompileHS.build
 productOfResult _ result       = ProductStatus (ppr result)


instance Spec Diff.Spec       Diff.Result where
 jobActionName _                = "diff"
 buildFromSpec                  = Diff.build
 productOfResult _ r
  = case r of
        Diff.ResultSame                 
         -> ProductStatus (ppr r)

        Diff.ResultDiff ref out diff    
         -> ProductDiff ref out diff


instance Spec RunDCX.Spec     RunDCX.Result where
 jobActionName _                = "run"
 buildFromSpec                  = RunDCX.build
 productOfResult _ result = ProductStatus (ppr result)


instance Spec RunExe.Spec     RunExe.Result where
 jobActionName _                = "run"
 buildFromSpec                  = RunExe.build
 productOfResult _ result       = ProductStatus (ppr result)


instance Spec Shell.Spec      Shell.Result where
 jobActionName _                = "shell"
 buildFromSpec                  = Shell.build
 productOfResult _ result       = ProductStatus (ppr result)




