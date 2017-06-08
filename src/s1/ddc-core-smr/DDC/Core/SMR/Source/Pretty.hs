{-# LANGUAGE GADTs #-}
module DDC.Core.SMR.Source.Pretty
where
import DDC.Core.SMR.Core.Exp
import qualified Data.Text  as T
import DDC.Data.Pretty

-- | Context of a pretty printed expression.
data Context where
        ContextFun    :: Context
        ContextArg    :: Context
        ContextBody   :: Context


instance Pretty (Module Name Name) where
 ppr (Module ds)
  = vcat $ map ppr ds


instance Pretty (Decl   Name Name) where
 ppr (DTerm n x)
  =  ppr (RMac n :: Ref Name Name) 
  <> line
  <> (indent 1 $ text "= " <+> pprExp' ContextArg x <> text ";" <> line)


instance Pretty (Exp    Name Name) where
 ppr xx = pprExp' ContextBody xx


-- | Pretty print an expression in the given context.
pprExp' (ctx :: Context) (xx :: Exp Name Name)
 | ContextBody  <- ctx
 = case xx of
        XVar name 0     -> text $ T.unpack name 
        XVar name d     -> ppr name <> text "^" <> (text $ show d)

        XAbs name x     -> text "\\"   <> ppr name <> text "." <> pprExp' ContextBody x

        XApp x1 x2
         -> case takeXApps xx of 
                Just (x1', [x2'])
                  -> pprExp' ContextFun x1' 
                  <> text " " 
                  <> pprExp' ContextArg x2'

                Just (x1', xs)
                  -> pprExp' ContextFun x1' 
                  <> line <> (indent 4 $  vcat (map (pprExp' ContextArg) xs))

                _ -> pprExp' ContextFun x1 <> text " " <> pprExp' ContextArg x2



        XRef r          -> ppr r

 | ContextFun   <- ctx
 = case xx of
        XAbs _ _        -> parens $ pprExp' ContextBody xx
        _               -> pprExp' ContextBody xx

 | ContextArg   <- ctx
 = case xx of
        XAbs _ _        -> parens $ pprExp' ContextBody xx
        XApp _ _        -> parens $ pprExp' ContextBody xx
        _               -> pprExp' ContextBody xx


instance Pretty Name where
 ppr n = text $ T.unpack n


instance Pretty (Ref Name Name) where
 ppr rr
  = case rr of
        RMac   n        -> text "@" <> ppr n
        RSet   n        -> text "+" <> ppr n
        RSym   s        -> text "%" <> ppr s
        RPrm   p        -> text "#" <> ppr p
