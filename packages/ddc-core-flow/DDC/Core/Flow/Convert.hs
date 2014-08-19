
-- | Conversion of Flow to Tetra
--
module DDC.Core.Flow.Convert
        ( tetraOfFlowModule )
where

import DDC.Core.Flow.Convert.Base
import DDC.Core.Flow.Convert.Type
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Exp

import qualified DDC.Core.Flow.Prim      as F
import qualified DDC.Core.Tetra.Prim     as T

import Control.Applicative


tetraOfFlowModule :: Module a F.Name -> Either Error (Module a T.Name)
tetraOfFlowModule mm
 = evalCheck ()
 $ convertM  mm

convertM :: Module a F.Name -> ConvertM (Module a T.Name)
convertM mm
 = 


convertX :: Exp a F.Name -> ConvertM (Exp a T.Name)

