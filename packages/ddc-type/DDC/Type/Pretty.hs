{-# OPTIONS_HADDOCK hide #-}
module DDC.Type.Pretty 
        (module DDC.Base.Pretty)
where
import DDC.Type.Exp
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Base.Pretty
import qualified DDC.Type.Sum           as Sum

stage   = "DDC.Type.Pretty"

-- Bind -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Bind n) where
 ppr bb
  = case bb of
        BName v t       -> ppr v     <+> text ":" <+> ppr t
        BAnon   t       -> text "^"  <+> text ":" <+> ppr t
        BNone   t       -> text "_"  <+> text ":" <+> ppr t


-- Binder ---------------------------------------------------------------------
instance Pretty n => Pretty (Binder n) where
 ppr bb
  = case bb of
        RName v         -> ppr v
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Pretty print a binder, adding spaces after names.
--   The RAnon and None binders don't need spaces, as they're single symbols.
pprBinderSep   :: Pretty n => Binder n -> Doc
pprBinderSep bb
 = case bb of
        RName v         -> ppr v
        RAnon           -> text "^"
        RNone           -> text "_"


-- | Print a group of binders with the same type.
pprBinderGroup :: (Pretty n, Eq n) => ([Binder n], Type n) -> Doc
pprBinderGroup (rs, t)
        =  (brackets $ (sep $ map pprBinderSep rs) <+> text ":"  <+> ppr t) 
        <> dot


-- Bound ----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Bound n) where
 ppr nn
  = case nn of
--        UName n t       -> parens (ppr n <> text ":" <> ppr t)
        UName n _       -> ppr n


        UPrim n _       -> ppr n
--        UIx i t         -> parens (text "^" <> ppr i <> text ":" <> ppr t)
        UIx i _         -> text "^" <> ppr i


-- Type -----------------------------------------------------------------------
instance (Pretty n, Eq n) => Pretty (Type n) where
 pprPrec d tt
  = case tt of
        -- Full application of function constructors are printed infix.
        TApp (TApp (TCon (TyConKind KiConFun)) k1) k2
         -> pprParen (d > 5)
         $  ppr k1 <+> text "~>" <+> ppr k2

        TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2
         -> pprParen (d > 5)
         $  pprPrec 6 t1 <+> text "=>" </> pprPrec 5 t2

        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFun)) t1) eff) clo) t2
         | isBot eff, isBot clo
         -> pprParen (d > 5)
         $  pprPrec 6 t1 <+> text "->"  </> pprPrec 5 t2

         | otherwise
         -> pprParen (d > 5)
         $  pprPrec 6 t1
                <+> text "-(" <> ppr eff <> text " | " <> ppr clo <> text ")>" 
                </> pprPrec 5 t2
                   
        -- Standard types.
        TCon tc    -> ppr tc
        TVar b     -> ppr b

        TForall b t
         | Just (bsMore, tBody) <- takeTForalls t
         -> let groups  = partitionBindsByType (b:bsMore)
            in  pprParen (d > 5) 
                 $ (cat $ map pprBinderGroup groups) <> ppr tBody
                        
         | otherwise
         -> pprParen (d > 5)
                $ brackets (ppr b) <> dot <> ppr t

        TApp t1 t2
         -> pprParen (d > 10)
         $  ppr t1 <+> pprPrec 11 t2

        TSum ts
         | isBot tt     
         -> ppr (Sum.kindOfSum ts) <> text "0"
         
         | otherwise
         -> pprParen (d > 9) $  ppr ts


instance (Pretty n, Eq n) => Pretty (TypeSum n) where
 ppr ss
  = case Sum.toList ss of
      [] | isEffectKind  $ Sum.kindOfSum ss -> text "!0"
         | isClosureKind $ Sum.kindOfSum ss -> text "$0"
         | isDataKind    $ Sum.kindOfSum ss -> text "*0"
         | otherwise     -> error $ stage ++ ": malformed sum"
         
      ts  -> sep $ punctuate (text " +") (map ppr ts)


-- TyCon ----------------------------------------------------------------------
instance (Eq n, Pretty n) => Pretty (TyCon n) where
 ppr tt
  = case tt of
        TyConSort sc    -> ppr sc
        TyConKind kc    -> ppr kc
        TyConWitness tc -> ppr tc
        TyConSpec tc    -> ppr tc
        TyConBound u    -> ppr u


instance Pretty SoCon where
 ppr sc 
  = case sc of
        SoConComp       -> text "**"
        SoConProp       -> text "@@"


instance Pretty KiCon where
 ppr kc
  = case kc of
        KiConFun        -> text "(~>)"
        KiConData       -> text "*"
        KiConRegion     -> text "%"
        KiConEffect     -> text "!"
        KiConClosure    -> text "$"
        KiConWitness    -> text "@"


instance Pretty TwCon where
 ppr tw
  = case tw of
        TwConImpl       -> text "(=>)"
        TwConPure       -> text "Pure"
        TwConEmpty      -> text "Empty"
        TwConGlobal     -> text "Global"
        TwConDeepGlobal -> text "DeepGlobal"
        TwConConst      -> text "Const"
        TwConDeepConst  -> text "DeepConst"
        TwConMutable    -> text "Mutable"
        TwConDeepMutable-> text "DeepMutable"
        TwConLazy       -> text "Lazy"
        TwConHeadLazy   -> text "HeadLazy"
        TwConManifest   -> text "Manifest"
        

instance Pretty TcCon where
 ppr tc 
  = case tc of
        TcConFun        -> text "(->)"
        TcConRead       -> text "Read"
        TcConHeadRead   -> text "HeadRead"
        TcConDeepRead   -> text "DeepRead"
        TcConWrite      -> text "Write"
        TcConDeepWrite  -> text "DeepWrite"
        TcConAlloc      -> text "Alloc"
        TcConDeepAlloc  -> text "DeepAlloc"
        TcConUse        -> text "Use"
        TcConDeepUse    -> text "DeepUse"


