{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Parser.Type
        ( pBind
        , pType
        , pTypeSum
        , pTypeApp
        , pTypeAtomSP
        , pTyConSP
        , pTyConBound)
where
import DDC.Source.Tetra.Lexer           as S
import DDC.Source.Tetra.Parser.Base     as S
import DDC.Source.Tetra.Exp.Source      as S
import DDC.Source.Tetra.Prim.TyConTetra as S
import DDC.Core.Lexer.Tokens            as K

import qualified DDC.Core.Tetra         as C
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as T


-- | Parse a binder.
pBind :: Parser Bind
pBind
 = P.choice
        -- Named binders.
        [ do    (b, _)  <- pBindNameSP
                return  $  b
                
        -- Anonymous binders.
        , do    pTok KHat
                return  $  BAnon 
        
        -- Vacant binders.
        , do    pTok KUnderscore
                return  $  BNone ]
 <?> "a binder"

-- | Parse a type.
pType :: Parser Type
pType = pTypeSum


-- | Parse a type sum.
pTypeSum :: Parser Type
pTypeSum
 = do   t1      <- pTypeForall
        P.choice 
         [ -- Type sums.
           -- T2 + T3
           do   sp      <- pTokSP (KOp "+")
                t2      <- pTypeSum
                return  $  TAnnot sp $ TSum KEffect t1 t2

         , do   return t1 ]
 <?> "a type"


-- | Parse a quantified type.
pTypeForall :: Parser Type
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
                return  $ foldr (\b t   -> TAnnot sp 
                                        $  TApp (TCon (TyConForall kBind)) 
                                                (TAbs b kBind t)) 
                                tBody bs

           -- Body type
         , do   pTypeFun
         ]
 <?> "a type"


-- | Parse a function type.
pTypeFun :: Parser Type
pTypeFun
 = do   t1      <- pTypeApp
        P.choice 
         [ -- T1 ~> T2
           do   sp      <- pTokSP KArrowTilde
                t2      <- pTypeForall
                return  $  TAnnot sp $ TFun t1 t2

           -- T1 => T2
         , do   sp      <- pTokSP KArrowEquals
                t2      <- pTypeForall
                return  $  TAnnot sp $ TImpl t1 t2

           -- T1 -> T2
         , do   sp      <- pTokSP KArrowDash
                t2      <- pTypeForall
                return  $  TAnnot sp $ TFun  t1 t2

           -- Body type
         , do   return t1 
         ]
 <?> "an atomic type or type application"


-- | Parse a type application.
pTypeApp :: Parser Type
pTypeApp
 = do   ((t, _):ts)  <- P.many1 pTypeAtomSP
        return  $  foldl (\t1 (t2, sp) -> TAnnot sp (TApp t1 t2)) t ts
 <?> "an atomic type or type application"


-- | Parse a variable, constructor or parenthesised type.
pTypeAtomSP :: Parser (Type, SourcePos)
pTypeAtomSP
 = P.choice
        -- (~>) and (=>) and (->) and (TYPE2)
        [ -- (~>)
          do    sp      <- pTokSP $ KOpVar "~>"
                return  (TAnnot sp $ TCon  TyConFun,  sp)

          -- (=>)
        , do    sp      <- pTokSP $ KOpVar "=>"
                return  (TAnnot sp $ TCon (TyConPrim (PrimTypeTwCon TwConImpl)), sp)

          -- (->)
        , do    sp      <- pTokSP $ KOpVar "->"
                return  (TAnnot sp $ TCon TyConFun,  sp)

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
                return  (TAnnot sp $ TBot KEffect, sp)

        -- Bound occurrence of a variable.
        --  We don't know the kind of this variable yet, so fill in the
        --  field with the bottom element of computation kinds. This isn't
        --  really part of the language, but makes sense implentation-wise.
        , do    (u, sp) <- pBoundNameSP
                return  (TAnnot sp $ TVar u, sp)

        , do    (u, sp) <- pBoundIxSP
                return  (TAnnot sp $ TVar u, sp)
        ]
 <?> "an atomic type"


-- | Parse a type constructor.
pTyConSP :: Parser (TyCon, SourcePos)
pTyConSP  =   P.pTokMaybeSP f <?> "a type constructor"
 where f kk
        = case kk of
                -- Primitive Ambient TyCons.
                KA (KSoConBuiltin c)    
                 -> Just $ TyConPrim $ PrimTypeSoCon c

                KA (KKiConBuiltin c)    
                 -> Just $ TyConPrim $ PrimTypeKiCon c

                KA (KTwConBuiltin c)
                 -> Just $ TyConPrim $ PrimTypeTwCon c

                KA (KTcConBuiltin c)
                 -> Just $ TyConPrim $ PrimTypeTcCon c

                -- Primitive TyCons.
                KN (KCon (NamePrimType tc))
                 -> Just $ TyConPrim tc

                -- User Bound TyCons.
                KN (KCon (NameCon tx))
                 -> Just (TyConBound (TyConBoundName tx))

                _ -> Nothing


-- | Parse a bound type constructor.
--   Known primitive type constructors do not match.
pTyConBound :: Parser TyCon
pTyConBound  
        =   P.pTokMaybe f <?> "a bound type constructor"
 where  
        f :: Tok Name -> Maybe TyCon
        f (KN (KCon (NameCon tx)))
         |  Nothing <- C.readPrimTyCon      (T.unpack tx)
         ,  Nothing <- S.readPrimTyConTetra (T.unpack tx)
         = Just (TyConBound (TyConBoundName tx))

        f _ = Nothing

