{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Parser.Type
        ( pBind
        , pTypeSum
        , pTypeApp
        , pTypeAtomSP
        , pTyConSP
        , pTyConBound)
where
import DDC.Source.Tetra.Exp.Source      as S
import DDC.Source.Tetra.Prim.TyConTetra as S
import DDC.Type.Exp.Generic.Exp         as T
import DDC.Core.Tetra                   as C
import DDC.Core.Lexer.Tokens            as C
import DDC.Core.Parser                  (Parser)
import DDC.Base.Parser                  ((<?>))
import Data.Text                        (Text)
import DDC.Base.Parser                  (SourcePos)
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as T


-- | Parse a binder.
pBind :: Parser Text Bind
pBind
 = P.choice
        -- Named binders.
        [ do    (v, _)  <- pVarSP
                return  $  BName v
                
        -- Anonymous binders.
        , do    pTok KHat
                return  $  BAnon 
        
        -- Vacant binders.
        , do    pTok KUnderscore
                return  $  BNone ]
 <?> "a binder"


--  | Parse a type sum.
pTypeSum :: Parser Text Type
pTypeSum
 = do   t1      <- pTypeForall
        P.choice 
         [ -- Type sums.
           -- T2 + T3
           do   sp      <- pTokSP (KOp "+")
                t2      <- pTypeSum
                return  $  TAnnot sp $ TSum TEffect t1 t2

         , do   return t1 ]
 <?> "a type"


-- | Parse a quantified type.
pTypeForall :: Parser Text Type
pTypeForall
 = P.choice
         [ -- Universal quantification.
           -- [v1 v1 ... vn : T1]. T2
           do   pTok KSquareBra
                bs      <- P.many1 pBind
                sp      <- pTokSP (KOp ":")
                kBind   <- pTypeSum
                pTok KSquareKet
                pTok KDot

                tBody   <- pTypeForall
                return  $ foldr (\b t -> TAnnot sp $ TForall kBind b t) tBody bs

           -- Body type
         , do   pTypeFun
         ]
 <?> "a type"


-- | Parse a function type.
pTypeFun :: Parser Text Type
pTypeFun
 = do   t1      <- pTypeApp
        P.choice 
         [ -- T1 ~> T2
           do   sp      <- pTokSP KArrowTilde
                t2      <- pTypeForall
                return  $  TAnnot sp $ TApp (TApp TKiFun  t1) t2

           -- T1 => T2
         , do   sp      <- pTokSP KArrowEquals
                t2      <- pTypeForall
                return  $  TAnnot sp $ TApp (TApp TDaImpl t1) t2

           -- T1 -> T2
         , do   sp      <- pTokSP KArrowDash
                t2      <- pTypeForall
                return  $  TAnnot sp $ TApp (TApp TDaFun  t1) t2

           -- Body type
         , do   return t1 
         ]
 <?> "an atomic type or type application"


-- | Parse a type application.
pTypeApp :: Parser Text Type
pTypeApp
 = do   ((t, _):ts)  <- P.many1 pTypeAtomSP
        return  $  foldl (\t1 (t2, sp) -> TAnnot sp (TApp t1 t2)) t ts
 <?> "an atomic type or type application"


-- | Parse a variable, constructor or parenthesised type.
pTypeAtomSP :: Parser Text (Type, SourcePos)
pTypeAtomSP
 = P.choice
        -- (~>) and (=>) and (->) and (TYPE2)
        [ -- (~>)
          do    sp      <- pTokSP $ KOpVar "~>"
                return  (TAnnot sp $ TKiFun,  sp)

          -- (=>)
        , do    sp      <- pTokSP $ KOpVar "=>"
                return  (TAnnot sp $ TDaImpl, sp)

          -- (->)
        , do    sp      <- pTokSP $ KOpVar "->"
                return  (TAnnot sp $ TDaFun,  sp)

          -- (TYPE2)
        , do    sp      <- pTokSP KRoundBra
                t       <- pTypeSum
                pTok KRoundKet
                return  (t, sp)

        -- Named type constructors
        , do    (tc, sp) <- pTyConSP 
                return  (TAnnot sp $ TCon tc, sp)
            
        -- Bottoms.
        , do    sp       <- pTokSP KBotEffect  
                return  (TAnnot sp $ TBot TEffect, sp)

        -- Bound occurrence of a variable.
        --  We don't know the kind of this variable yet, so fill in the
        --  field with the bottom element of computation kinds. This isn't
        --  really part of the language, but makes sense implentation-wise.
        , do    (v, sp) <- pVarSP
                return  (TAnnot sp $ TVar (UName v), sp)

        , do    (i, sp) <- pIndexSP
                return  (TAnnot sp $ TVar (UIx i), sp)
        ]
 <?> "an atomic type"


-- | Parse a type constructor.
pTyConSP :: Parser Text (TyCon, SourcePos)
pTyConSP  =   P.pTokMaybeSP f 
        <?> "a type constructor"

 where f kk
        = case kk of
                -- Primitive Ambient TyCons.
                KA (KSoConBuiltin c)    
                 -> Just $ TyConPrim $ TyConPrimSoCon c

                KA (KKiConBuiltin c)    
                 -> Just $ TyConPrim $ TyConPrimKiCon c

                KA (KTwConBuiltin c)
                 -> Just $ TyConPrim $ TyConPrimTwCon c

                KA (KTcConBuiltin c)
                 -> Just $ TyConPrim $ TyConPrimTcCon c

                -- Primitive Machine TyCons.
                KN (KCon tx)
                 |  Just tc  <- C.readPrimTyCon (T.unpack tx)
                 -> Just $ TyConPrim $ TyConPrimMach  tc

                -- Primitive Tetra TyCons.
                KN (KCon tx)
                 |  Just tc  <- S.readPrimTyConTetra (T.unpack tx)
                 -> Just $ TyConPrim $ TyConPrimTetra tc

                -- User Bound TyCons.
                KN (KCon tx)
                 -> Just (TyConBound tx)

                _ -> Nothing


-- | Parse a bound type constructor.
--   Known primitive type constructors do not match.
pTyConBound :: Parser Text TyCon
pTyConBound  
        =   P.pTokMaybe f
        <?> "a bound type constructor"
 where  
        f :: Tok Text -> Maybe TyCon
        f (KN (KCon tx))          
         |  Nothing <- C.readPrimTyCon      (T.unpack tx)
         ,  Nothing <- S.readPrimTyConTetra (T.unpack tx)
         = Just (TyConBound tx)

        f _ = Nothing


-- | Parse a deBruijn index, with source position.
pIndexSP ::  Parser n (Int, SourcePos)
pIndexSP =   P.pTokMaybeSP f
         <?> "an index"
 where  f (KA (KIndex i))       = Just i
        f _                     = Nothing


-- | Parse a variable, with source position.
pVarSP  ::  Parser n (n, SourcePos)
pVarSP  =   P.pTokMaybeSP f
        <?> "a variable"
 where  f (KN (KVar n))         = Just n
        f _                     = Nothing


-- | Parse an atomic token, yielding its source position.
pTokSP :: TokAtom -> Parser n SourcePos
pTokSP k   = P.pTokSP (KA k)


-- | Parse an atomic token.
pTok :: TokAtom -> Parser n ()
pTok k     = P.pTok (KA k)

