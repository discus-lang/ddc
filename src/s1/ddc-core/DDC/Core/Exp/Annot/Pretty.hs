{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeFamilies #-}
-- | Pretty printing for annotated expressions.
module DDC.Core.Exp.Annot.Pretty
        ( module DDC.Data.Pretty
        , PrettyMode (..))
where
import DDC.Core.Exp.Annot
import DDC.Type.Exp.Simple.Pretty       ()
import DDC.Data.Pretty
import Data.List
import qualified Data.Text              as Text
import Prelude                          hiding ((<$>))


-- Exp ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Exp a n) where
 data PrettyMode (Exp a n)
        = PrettyModeExp
        { modeExpLets           :: PrettyMode (Lets a n)
        , modeExpAlt            :: PrettyMode (Alt a n)

          -- Display types on primitive variables.
        , modeExpVarTypes       :: Bool

          -- Display types on primitive constructors.
        , modeExpConTypes       :: Bool

          -- Use 'letcase' for single alternative case expressions.
        , modeExpUseLetCase     :: Bool }


 pprDefaultMode
        = PrettyModeExp
        { modeExpLets           = pprDefaultMode
        , modeExpAlt            = pprDefaultMode
        , modeExpConTypes       = False
        , modeExpVarTypes       = False
        , modeExpUseLetCase     = False }


 pprModePrec mode d xx
  = let pprX    = pprModePrec mode 0
        pprLts  = pprModePrec (modeExpLets mode) 0
        pprAlt  = pprModePrec (modeExpAlt  mode) 0
    in case xx of

        XVar  _ u
         | modeExpVarTypes mode
         , Just t       <- takeTypeOfBound u
         -> parens $ ppr u <> text ":" <+> ppr t

         | otherwise
         -> ppr u

        XPrim _ p
         -> ppr p

        XCon  _ dc
         | modeExpConTypes mode
         , Just t       <- takeTypeOfDaCon dc
         -> parens $ ppr dc <> text ":" <+> ppr t

         | otherwise
         -> ppr dc

        XAbs _ (MType _) _
         -> let Just (bs, xBody) = takeXLAMs xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "Λ")) groups)
                 <>  (if      isXLAM    xBody then empty
                      else if isXLam    xBody then line
                      else if isSimpleX xBody then space
                      else    line)
                 <>  pprX xBody

        XAbs _ (MTerm _) _
         -> let Just (bs, xBody) = takeXLams xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "\955")) groups)
                 <> breakWhen (not $ isSimpleX xBody)
                 <> pprX xBody

        XAbs _ (MImplicit b) xBody
         -> pprParen' (d > 1)
                $  text "\955{" <> ppr b <> text "}."
                <> breakWhen (not $ isSimpleX xBody)
                <> pprX xBody

        XApp _ x1 (RTerm x2)
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> nest 4 (breakWhen (not $ isSimpleX x2)
                <> pprModePrec mode 11 x2)

        XApp _ x1 (RType x2)
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> text " [" <> ppr x2 <> text "]"

        XApp _ x1 (RWitness x2)
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> text " <" <> ppr x2 <> text ">"

        XApp _ x1 (RImplicit (RTerm x2))
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> nest 4 (breakWhen (not $ isSimpleX x2)
                <> text "{" <> ppr x2 <> text "}")

        XApp _ x1 a2
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                <> ppr a2

        XLet _ lts x
         ->  pprParen' (d > 2)
         $   pprLts lts <+> text "in"
         <$> pprX x

        -- Print single alternative case expressions as 'letcase'.
        --    case x1 of { C v1 v2 -> x2 }
        -- => letcase C v1 v2 <- x1 in x2
        XCase _ x1 [AAlt p x2]
         | modeExpUseLetCase mode
         ->  pprParen' (d > 2)
         $   text "letcase" <+> ppr p
                <+> nest 2 (breakWhen (not $ isSimpleX x1)
                            <> text "=" <+> align (pprX x1))
                <+> text "in"
         <$> pprX x2

        XCase _ x alts
         -> pprParen' (d > 2)
         $  (nest 2 $ text "case" <+> ppr x <+> text "of" <+> lbrace <> line
                <> (vcat $ punctuate semi $ map pprAlt alts))
         <> line
         <> rbrace

        XCast _ CastBox x
         -> pprParen' (d > 2)
         $  text "box"  <$> pprX x

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"  <+> pprX x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   ppr cc <+> text "in"
         <$> pprX x


-- Param ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Param n) where
 ppr mm
  = case mm of
        MType b         -> text "{*" <+> ppr b <> text "}"
        MTerm b         -> text "("  <+> ppr b <> text ")"
        MImplicit b     -> text "{"  <+> ppr b <> text "}"


-- Arg ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Arg a n) where
 ppr aa
  = case aa of
        RType t
         -> text "["  <> ppr t <> text "]"

        RTerm x
         -> text "("  <> ppr x <> text ")"

        RWitness w
         -> text "<"  <> ppr w <> text ">"

        -- An implicit term.
        RImplicit (RTerm x)
          -> text "{"  <> ppr x <> text "}"

        _ -> text "INVALID"


-- Prim -----------------------------------------------------------------------
instance Pretty Prim where
 ppr pp
  = case pp of
        PElaborate      -> text "elaborate#"
        PProject n      -> text "project(" <> text (Text.unpack n) <> text ")#"
        PShuffle        -> text "shuffle#"
        PCombine        -> text "combine#"


-- Pat ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Pat n) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u <+> sep (map pprPatBind bs)


-- | Pretty print a binder,
--   showing its type annotation only if it's not bottom.
pprPatBind :: (Eq n, Pretty n) => Bind n -> Doc
pprPatBind b
        | isBot (typeOfBind b)  = ppr $ binderOfBind b
        | otherwise             = parens $ ppr b


-- Alt ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Alt a n) where
 data PrettyMode (Alt a n)
        = PrettyModeAlt
        { modeAltExp            :: PrettyMode (Exp a n) }

 pprDefaultMode
        = PrettyModeAlt
        { modeAltExp            = pprDefaultMode }

 pprModePrec mode _ (AAlt p x)
  = let pprX    = pprModePrec (modeAltExp mode) 0
    in  ppr p <+> nest 1 (line <> nest 3 (text "->" <+> pprX x))


-- DaCon ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DaCon n (Type n)) where
 ppr dc
  = case dc of
        DaConUnit       -> text "()"

        DaConRecord ns
         -> text "("
         <> (hcat $ punctuate (text ",") $ map (text . Text.unpack) ns)
         <> text ")#"

        DaConPrim  n _  -> ppr n
        DaConBound n    -> ppr n


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff
         -> text "weakeff" <+> brackets (ppr eff)

        CastPurify w
         -> text "purify"  <+> angles   (ppr w)

        CastBox
         -> text "box"

        CastRun
         -> text "run"


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 data PrettyMode (Lets a n)
        = PrettyModeLets
        { modeLetsExp           :: PrettyMode (Exp a n)
        , modeLetsSuppressTypes :: Bool }

 pprDefaultMode
        = PrettyModeLets
        { modeLetsExp           = pprDefaultMode
        , modeLetsSuppressTypes = False }

 pprModePrec mode _ lts
  = let pprX    = pprModePrec (modeLetsExp mode) 0
    in case lts of
        LLet b x
         -> let bHasType = not $ isBot (typeOfBind b)

                dBind    = if modeLetsSuppressTypes mode || (not bHasType)
                             then ppr (binderOfBind b)
                             else ppr b

            in  text "let"
                 <+> align (  (padL 7 dBind)
                           <>  nest (if bHasType then 2 else 6 )
                                ( breakWhen bHasType
                                <> text "=" <+> align (pprX x)))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =   ppr (binderOfBind b)
                 <>  text ":" <+> ppr (typeOfBind b)
                 <>  nest 2 (  breakWhen (not $ isSimpleX x)
                            <> text "=" <+> align (pprX x))

           in   (nest 2 $ text "letrec"
                  <+> lbrace
                  <>  (  line
                      <> (vcat $ punctuate (semi <> line)
                               $ map pprLetRecBind bxs)))
                <$> rbrace

        LPrivate bs Nothing []
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))

        LPrivate bs Nothing bws
         -> text "private"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map pprWitBind bws)

        LPrivate bs (Just parent) []
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))

        LPrivate bs (Just parent) bws
         -> text "extend"
                <+> ppr parent
                <+> text "using"
                <+> (hcat $ punctuate space (map (ppr . binderOfBind) bs))
                <+> text "with"
                <+> braces (cat $ punctuate (text "; ") $ map pprWitBind bws)


-- | When we pretty print witness binders,
--   suppress the underscore when there is no name.
pprWitBind :: (Eq n, Pretty n) => Bind n -> Doc
pprWitBind b
 = case b of
        BNone t -> ppr t
        _       -> ppr b


-- Witness --------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Witness a n) where
 pprPrec d ww
  = case ww of
        WVar _ n        -> ppr n
        WCon _ wc       -> ppr wc
        WApp _ w1 w2    -> pprParen (d > 10) (ppr w1 <+> pprPrec 11 w2)
        WType _ t       -> text "[" <> ppr t <> text "]"


instance (Pretty n, Eq n) => Pretty (WiCon n) where
 ppr wc
  = case wc of
        WiConBound   u  _ -> ppr u


-- Binder ---------------------------------------------------------------------
-- | Preey print a binder.
pprBinder   :: Pretty n => Binder n -> Doc
pprBinder bb
 = case bb of
        RName v         -> ppr v
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Print a group of binders with the same type.
pprBinderGroup
        :: (Pretty n, Eq n)
        => Doc -> ([Binder n], Type n) -> Doc

pprBinderGroup lam (rs, t)
        =  lam
        <> parens ((hsep $ map pprBinder rs) <> text ":" <+> ppr t)
        <> dot


-- Utils ----------------------------------------------------------------------
-- | Produce an expression break character.
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


-- | Check if an expression is simple enough to not worry about
--   starting a separate line for.
isSimpleX :: Exp a n -> Bool
isSimpleX xx
 = case xx of
        XVar{}                   -> True
        XCon{}                   -> True
        XApp _ _  RType{}        -> True
        XApp _ x1 (RTerm x2)     -> isSimpleX x1 && isAtomX x2

        XApp _ x1 (RImplicit (RTerm x2))
                -> isSimpleX x1 && isAtomX x2

        XApp _ x1 (RWitness _w2)
                -> isSimpleX x1
        _       -> False


-- | Wrap a document in parenthesis.
parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c

