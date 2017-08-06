{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module DDC.Core.Exp.Generic.Pretty where
import DDC.Core.Exp.Generic.Predicates
import DDC.Core.Exp.Generic.Exp
import DDC.Core.Exp.DaCon
import DDC.Type.Exp.Simple      ()
import DDC.Type.Exp.Simple.Exp
import DDC.Data.Pretty
import qualified Data.Text      as Text
import Prelude                  hiding ((<$>))


-------------------------------------------------------------------------------
-- | Synonym for Pretty constraints on all language types.
type PrettyLanguage l
        = ( Eq l
          , Pretty l
          , Pretty (GAnnot l)
          , Pretty (GBind l), Pretty (GBound l), Pretty (GPrim l))


-- Exp ------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GExp l) where

 data PrettyMode (GExp l)
        = PrettyModeExp
        { -- | Mode to use when pretty printing arguments.
          modeExpArg            :: PrettyMode (GArg l)

          -- | Mode to use when pretty printing let expressions.
        , modeExpLets           :: PrettyMode (GLets l)

          -- | Mode to use when pretty printing alternatives.
        , modeExpAlt            :: PrettyMode (GAlt  l)

          -- | Use 'letcase' for single alternative case expressions.
        , modeExpUseLetCase     :: Bool }


 pprDefaultMode
        = PrettyModeExp
        { modeExpArg            = pprDefaultMode
        , modeExpLets           = pprDefaultMode
        , modeExpAlt            = pprDefaultMode
        , modeExpUseLetCase     = False }


 pprModePrec mode d xx
  = let pprX    = pprModePrec mode 0
        pprLts  = pprModePrec (modeExpLets mode) 0
        pprAlt  = pprModePrec (modeExpAlt  mode) 0

    in case xx of
        XAnnot _ x      -> ppr x
        XVar   u        -> ppr u
        XCon   dc       -> ppr dc
        XPrim  p        -> ppr p

        XAbs (MType b) xBody
         -> pprParen' (d > 1)
                $  text "/\\"
                <> ppr b
                <> (if       isXLAM    xBody then empty
                     else if isXLam    xBody then line <> space
                     else if isSimpleX xBody then space
                     else    line)
                <> pprX xBody

        XAbs (MTerm b) xBody
         -> pprParen' (d > 1)
                $  text "\\"
                <> ppr b
                <> breakWhen (not $ isSimpleX xBody)
                <> pprX xBody

        XApp x1 a2
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> nest 4 (breakWhen (not $ isSimpleR a2)
                          <> pprModePrec (modeExpArg mode) 11 a2)

        XLet lts x
         ->  pprParen' (d > 2)
         $   pprLts lts <+> text "in"
         <$> pprX x

        -- Print single alternative case expressions as 'letcase'.
        --    case x1 of { C v1 v2 -> x2 }
        -- => letcase C v1 v2 <- x1 in x2
        XCase x1 [AAlt p x2]
         | modeExpUseLetCase mode
         ->  pprParen' (d > 2)
         $   text "letcase" <+> ppr p
                <+> nest 2 (breakWhen (not $ isSimpleX x1)
                            <> text "=" <+> align (pprX x1))
                <+> text "in"
         <$> pprX x2

        XCase x alts
         -> pprParen' (d > 2)
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map pprAlt alts))
         <> line
         <> rbrace

        XCast CastBox x
         -> pprParen' (d > 2)
         $  text "box"  <$> pprX x

        XCast CastRun x
         -> pprParen' (d > 2)
         $  text "run"  <+> pprX x

        XCast cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> pprX x


-- Arg ------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GArg l) where

 data PrettyMode (GArg l)
        = PrettyModeArg
        { modeArgExp            :: PrettyMode (GExp l) }

 pprModePrec mode n aa
  = case aa of
        RType    t      -> text "[" <> ppr t <> text "]"
        RExp     x      -> pprModePrec (modeArgExp mode) n  x
        RWitness w      -> text "<" <> ppr w <> text ">"


-- Pat ------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GPat l) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map ppr bs)


-- Alt ------------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GAlt l) where
 data PrettyMode (GAlt l)
        = PrettyModeAlt
        { modeAltExp            :: PrettyMode (GExp l) }

 pprDefaultMode
        = PrettyModeAlt
        { modeAltExp            = pprDefaultMode }

 pprModePrec mode _ (AAlt p x)
  = let pprX    = pprModePrec (modeAltExp mode) 0
    in  ppr p <+> nest 1 (line <> nest 3 (text "->" <+> pprX x))


-- Cast -----------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GCast l) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff   -> text "weakeff" <+> brackets (ppr eff)
        CastPurify w            -> text "purify"  <+> angles   (ppr w)
        CastBox                 -> text "box"
        CastRun                 -> text "run"

        CastPrim pp
         -> case pp of
                CastPrimProject -> text "project#"
                CastPrimShuffle -> text "shuffle#"
                CastPrimCombine -> text "combine#"


-- Lets -----------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GLets l) where
 data PrettyMode (GLets l)
        = PrettyModeLets
        { modeLetsExp           :: PrettyMode (GExp l)  }

 pprDefaultMode
        = PrettyModeLets
        { modeLetsExp           = pprDefaultMode }

 pprModePrec mode _ lts
  = let pprX    = pprModePrec (modeLetsExp mode) 0
    in case lts of
        LLet b x
         ->  text "let"
         <+> align ( ppr b
                 <>  nest 2 ( text "=" <+> align (pprX x)))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =  ppr b
                 <> nest 2 (  breakWhen (not $ isSimpleX x)
                           <> text "=" <+> align (pprX x))

           in   (nest 2 $ text "letrec"
                  <+> lbrace
                  <>  (  line
                      <> (vcat $ punctuate (semi <> line)
                               $ map pprLetRecBind bxs)))
                <$> rbrace

        LPrivate bs Nothing []
         -> text "private"
                <+> (hcat $ punctuate space $ map ppr bs)

        LPrivate bs Nothing bws
         -> text "private"
                <+> (hcat $ punctuate space $ map ppr bs)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bws)

        LPrivate bs (Just parent) []
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space $ map ppr bs)

        LPrivate bs (Just parent) bws
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space $ map ppr bs)
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map ppr bws)


-- Witness --------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GWitness l) where
 pprPrec d ww
  = case ww of
        WVar  n         -> ppr n
        WCon  wc        -> ppr wc
        WApp  w1 w2     -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
        WType t         -> text "[" <> ppr t <> text "]"


-- WiCon ----------------------------------------------------------------------
instance PrettyLanguage l => Pretty (GWiCon l) where
 ppr wc
  = case wc of
        WiConBound u  _ -> ppr u


-- DaCon ----------------------------------------------------------------------
instance PrettyLanguage l => Pretty (DaCon l (Type l)) where
 ppr dc
  = case dc of
        DaConUnit       -> text "()"

        DaConRecord ns
         -> text "("
         <> (hcat $ punctuate (text ",") $ map (text . Text.unpack) ns)
         <> text ")"

        DaConPrim  n _  -> ppr n
        DaConBound n    -> ppr n


-- Utils ----------------------------------------------------------------------
-- | Insert a line or a space depending on a boolean argument.
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


-- | Wrap a `Doc` in parens, and indent it one level.
parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c


-- | Check if this is a simple expression that does not need extra spacing when
--   being pretty printed.
isSimpleX :: GExp l -> Bool
isSimpleX xx
 = case xx of
        XVar{}          -> True
        XPrim{}         -> True
        XCon{}          -> True
        XApp x1 a2      -> isSimpleX x1 && isAtomR a2
        _               -> False

-- | Check if this is a simple argument that does not need extra spacing when
--   being pretty printed.
isSimpleR :: GArg l -> Bool
isSimpleR aa
 = case aa of
        RType{}         -> True
        RExp x          -> isSimpleX x
        RWitness{}      -> True


