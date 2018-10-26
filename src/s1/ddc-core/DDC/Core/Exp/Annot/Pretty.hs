{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeFamilies #-}
-- | Pretty printing for annotated expressions.
module DDC.Core.Exp.Annot.Pretty
        ( module DDC.Data.Pretty
        , PrettyMode (..))
where
import DDC.Core.Codec.Text.Pretty.Type  ()
import DDC.Core.Exp.Annot
import DDC.Data.Pretty
import DDC.Data.Label


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
        pprLts  = pprModePrec (modeExpLets mode) { modeLetsExp = mode } 0
        pprAlt  = pprModePrec (modeExpAlt  mode) { modeAltExp  = mode } 0
    in case xx of

        XVar  _ u  -> ppr u

        XAbs _ (MType _) _
         -> let Just (bs, xBody) = takeXLAMs xx
                groups = partitionBindsByType bs
            in  pprParen' (d > 1)
                 $  (cat $ map (pprBinderGroup (text "Λ")) groups)
                 <>  (if      isXLAM    xBody then mempty
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
                % text " <" % ppr x2 % text ">"

        XApp _ x1 (RImplicit (RTerm x2))
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1
                % nest 4 (breakWhen (not $ isSimpleX x2)
                         % text "{" % ppr x2 % text "}")

        XApp _ x1 a2
         -> pprParen' (d > 10)
         $  pprModePrec mode 10 x1 % ppr a2

        XLet _ lts x
         ->  pprParen' (d > 2)
         $   vcat
                [ pprLts lts %% text "in"
                , pprX x]

        XAtom _ (MAPrim p)  -> ppr p
        XAtom _ (MACon dc)  -> ppr dc
        XAtom _ (MALabel l) -> text "~" <> ppr l

        -- Print single alternative case expressions as 'letcase'.
        --    case x1 of { C v1 v2 -> x2 }
        -- => letcase C v1 v2 <- x1 in x2
        XCase _ x1 [AAlt p x2]
         | modeExpUseLetCase mode
         ->  pprParen' (d > 2)
         $   vcat
                [ text "letcase" %% ppr p
                        %% nest 2 (breakWhen (not $ isSimpleX x1)
                              % text "=" %% align (pprX x1))
                        %% text "in"
                , pprX x2]

        XCase _ x alts
         -> pprParen' (d > 2)
         $  (nest 2 $ text "case" %% ppr x %% text "of" %% lbrace % line
                % (vcat $ punctuate semi $ map pprAlt alts))
         % line
         % rbrace

        XCast _ CastBox x
         -> pprParen' (d > 2)
         $  vcat [text "box", pprX x]

        XCast _ CastRun x
         -> pprParen' (d > 2)
         $  text "run"  %% pprX x

        XCast _ cc x
         ->  pprParen' (d > 2)
         $   vcat [ ppr cc %% text "in", pprX x ]

        -- TODO FIXME I have no idea what these magic numbers mean.
        XAsync _ b e1 e2
         -> pprParen' (d > 2)
         $  vcat [
                    text "async",
                    ppr b,
                    text "<-",
                    pprX e1,
                    text "in",
                    pprX e2
                 ]

-- Param ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Param n) where
 ppr mm
  = case mm of
        MType b         -> text "{*" %% ppr b % text "}"
        MTerm b         -> text "("  %% ppr b % text ")"
        MImplicit b     -> text "{"  %% ppr b % text "}"


-- Arg ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Arg a n) where
 ppr aa
  = case aa of
        RType t         -> text "["  <> ppr t <> text "]"
        RTerm x         -> text "("  <> ppr x <> text ")"
        RWitness w      -> text "<"  <> ppr w <> text ">"

        -- An implicit term.
        RImplicit (RTerm x)
          -> text "{"  <> ppr x <> text "}"

        _               -> text "INVALID"


-- Prim -----------------------------------------------------------------------
instance Pretty Prim where
 ppr pp
  = case pp of
        PElaborate
         -> text "elaborate#"

        PTuple ls
         -> text "tuple#"
         %% (braces $ hcat (punctuate (text ",") (map ppr ls)))

        PRecord ls
         -> text "record#"
         %% (braces $ hcat (punctuate (text ",") (map ppr ls)))

        PProject l
         -> text "project#" %% (braces $ ppr l)

        PVariant l
         -> text "variant#" %% (braces $ ppr l)


-- Label ----------------------------------------------------------------------
instance Pretty Label where
 ppr ll = text (nameOfLabel ll)


-- Pat ------------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Pat n) where
 ppr pp
  = case pp of
        PDefault        -> text "_"
        PData u bs      -> ppr u %% sep (map pprPatBind bs)


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
    in  ppr p %% nest 1 (line % nest 3 (text "->" %% pprX x))


-- DaCon ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (DaCon n (Type n)) where
 ppr dc
  = case dc of
        DaConUnit       -> text "()"

        DaConRecord ns
         -> text "("
         <> (hcat $ punctuate (text ",") $ map text ns)
         <> text ")#"

        DaConPrim  n    -> ppr n
        DaConBound n    -> ppr n


instance (Pretty n, Eq n) => Pretty (DaConBoundName n) where
 ppr dc
  = case dc of
        DaConBoundName Nothing Nothing n
         -> ppr n

        DaConBoundName Nothing (Just nd) n
         -> ppr nd % text "." % ppr n

        DaConBoundName (Just mn) md n
         -> ppr mn % text "." % maybe (text "") ppr md % text "." % ppr n


-- Cast -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Cast a n) where
 ppr cc
  = case cc of
        CastWeakenEffect  eff
         -> text "weakeff" %% brackets (ppr eff)

        CastPurify w
         -> text "purify"  %% angles   (ppr w)

        CastBox
         -> text "box"

        CastRun
         -> text "run"


-- Lets -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Lets a n) where
 data PrettyMode (Lets a n)
        = PrettyModeLets
        { modeLetsExp           :: PrettyMode (Exp a n)
          -- | Suppress type annotations on let-bindings.
        , modeLetsSuppressTypes :: Bool
          -- | Try to keep types of binders in their own column.
        , modeLetsColumnTypes   :: Bool }

 pprDefaultMode
        = PrettyModeLets
        { modeLetsExp           = pprDefaultMode
        , modeLetsSuppressTypes = False
        , modeLetsColumnTypes   = False }

 pprModePrec mode _ lts
  = let pprX    = pprModePrec (modeLetsExp mode) { modeExpLets = mode } 0
    in case lts of
        LLet b x
         -> let bHasType = not $ isBot (typeOfBind b)

                dBind
                 | not bHasType
                 = ppr (binderOfBind b)

                 | modeLetsColumnTypes mode
                 = padL 8 (ppr (binderOfBind b)) % text ":" %% ppr (typeOfBind b)

                 | modeLetsSuppressTypes mode
                 = ppr (binderOfBind b)

                 | otherwise = ppr b

                nBindColumnWidth
                 = if modeLetsColumnTypes mode
                        then (26 :: Int)
                        else  8

                nBindWidth
                 = length $ renderPlain dBind

                bBreakForExp
                 | modeLetsColumnTypes mode
                 = nBindWidth >= nBindColumnWidth

                 | otherwise
                 = bHasType

            in  text "let"
                 %% align ( (padL nBindColumnWidth dBind)
                          % breakWhen bBreakForExp
                          % string "=" %% align (pprX x))

        LRec bxs
         -> let pprLetRecBind (b, x)
                 =  ppr (binderOfBind b)
                 %  text ":" %% ppr (typeOfBind b)
                 %  nest 2 ( breakWhen (not $ isSimpleX x)
                           % text "=" %% align (pprX x))

           in vcat
                [ nest 2
                        $ text "letrec"
                        %% lbrace %  line
                        %  (vcat  $ punctuate (semi <> line)
                                  $ map pprLetRecBind bxs)
                , rbrace ]

        LPrivate bs Nothing []
         -> hsep
                [ text "private"
                , hcat $ punctuate space (map (ppr . binderOfBind) bs) ]

        LPrivate bs Nothing bws
         -> hsep
                [ text "private"
                , hcat $ punctuate space (map (ppr . binderOfBind) bs)
                , text "with"
                , braces (cat $ punctuate (text "; ") $ map pprWitBind bws) ]

        LPrivate bs (Just parent) []
         -> hsep
                [ text "extend"
                , ppr parent
                , text "using"
                , hcat $ punctuate space (map (ppr . binderOfBind) bs) ]

        LPrivate bs (Just parent) bws
         -> hsep
                [ text "extend"
                , ppr parent
                , text "using"
                , hcat $ punctuate space (map (ppr . binderOfBind) bs)
                , text "with"
                , braces (cat $ punctuate (string "; ") $ map pprWitBind bws) ]


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
        WApp _ w1 w2    -> pprParen (d > 10) (ppr w1 %% pprPrec 11 w2)
        WType _ t       -> char '[' % ppr t % char ']'


instance (Pretty n, Eq n) => Pretty (WiCon n) where
 ppr wc
  = case wc of
        WiConBound   u  _ -> ppr u


-- Binder ---------------------------------------------------------------------
-- | Pretty print a binder.
pprBinder   :: Pretty n => Binder n -> Doc
pprBinder bb
 = case bb of
        RName v         -> ppr v
        RAnon           -> char '^'
        RNone           -> char '_'


-- | Print a group of binders with the same type.
pprBinderGroup
        :: (Pretty n, Eq n)
        => Doc -> ([Binder n], Type n) -> Doc

pprBinderGroup lam (rs, t)
        = lam
        % parens ((hsep $ map pprBinder rs) % string ":" %% ppr t)
        % dot


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

