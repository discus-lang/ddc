
module DDC.Core.Discus.Convert.Exp.PrimInfo
        (convertPrimInfo)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import qualified Data.Monoid             as T
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Discus.Prim    as D
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A


convertPrimInfo
        :: Show a
        => ExpContext                    -- ^ The surrounding expression context.
        -> Context a                     -- ^ Types and values in the environment.
        -> Exp (AnTEC a D.Name) (D.Name) -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimInfo _ectx ctx xxExp
 = let  convertX = contextConvertExp ctx
   in case xxExp of

        XApp a _xa _xb
         | Just ( D.NameOpInfo D.OpInfoFrameNew True
                , [RTerm xLength])
                <- takeXFragApps xxExp
         -> Just $ do
                let a'   =  annotTail a
                xLength' <- convertX ExpArg ctx xLength
                return   $ xApps a' (XVar a' (UName (A.NameVar "ddcInfoFrameNew")))
                                    [RTerm xLength']

        XApp a _xa _xb
         | Just ( D.NameOpInfo D.OpInfoFramePush True
                , [RTerm xAddr])
                <- takeXFragApps xxExp
         -> Just $ do
                let a'   =  annotTail a
                xAddr'   <- convertX ExpArg ctx xAddr
                return   $ xApps a' (XVar a' (UName (A.NameVar "ddcInfoFramePush")))
                                    [RTerm xAddr']

        XApp a _xa _xb
         | Just ( D.NameOpInfo D.OpInfoFrameAddData True
                , [ RTerm xAddr
                  , RTerm xTag
                  , RTerm xArity
                  , RTerm xTxtModule@(XCon _ dcTxtModuleName)
                  , RTerm xTxtCon   @(XCon _ dcTxtCtorName) ])
                <- takeXFragApps xxExp
         , D.NameLitUnboxed (D.NameLitTextLit txModuleName)
                <- daConName dcTxtModuleName
         , D.NameLitUnboxed (D.NameLitTextLit txCtorName)
                <- daConName dcTxtCtorName
         -> Just $ do
                let a'   =  annotTail a
                xAddr'      <- convertX ExpArg ctx xAddr
                xTag'       <- convertX ExpArg ctx xTag
                xArith'     <- convertX ExpArg ctx xArity
                xTxtModule' <- convertX ExpArg ctx xTxtModule
                xTxtCon'    <- convertX ExpArg ctx xTxtCon

                let txSymbolInfoIndex
                        = "ddcInfoIndex." T.<> txModuleName
                        T.<> "." T.<> txCtorName

                return
                 $ xLets a'
                        [ LLet  (BAnon $ A.tWord 32)
                          $ xApps a' (XVar a' (UName (A.NameVar "ddcInfoFrameAddData")))
                                [ RTerm xAddr', RTerm xTag', RTerm xArith'
                                , RTerm xTxtModule', RTerm xTxtCon' ]

                        , LLet  (BNone $ A.tVoid)
                          $ A.xWrite a'
                                (A.tWord 32)
                                (A.xGlobali a' (A.tWord 32) txSymbolInfoIndex)
                                (A.xNat a' 0)
                                (XVar a' (UIx 0)) ]
                 $ XVar a' (UIx 0)

        _ -> Nothing

