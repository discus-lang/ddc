
-- | Parser for type expressions.
module DDC.Core.Parser.Type
        ( pType
        , pTypeAtom
        , pTypeApp
        , pBinder
        , pIndex
        , pTok
        , pTokAs)
where
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens   
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Base.Parser                  ((<?>))
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Sum           as TS


-- | Parse a type.
pType   :: Ord n 
        => Context -> Parser n (Type n)

pType c  
 =      pTypeSum c
 <?> "a type"


--  | Parse a type sum.
pTypeSum 
        :: Ord n 
        => Context -> Parser n (Type n)
pTypeSum c
 = do   t1      <- pTypeForall c
        P.choice 
         [ -- Type sums.
           -- T2 + T3
           do   pTok (KOp "+")
                t2      <- pTypeSum c
                return  $ TSum $ TS.fromList (tBot sComp) [t1, t2]
                
         , do   return t1 ]
 <?> "a type"


-- | Parse a binder.
pBinder :: Ord n => Parser n (Binder n)
pBinder
 = P.choice
        -- Named binders.
        [ do    v       <- pVar
                return  $ RName v
                
        -- Anonymous binders.
        , do    pTok KHat
                return  $ RAnon 
        
        -- Vacant binders.
        , do    pTok KUnderscore
                return  $ RNone ]
 <?> "a binder"


-- | Parse a quantified type.
pTypeForall 
        :: Ord n 
        => Context -> Parser n (Type n)
pTypeForall c
 = P.choice
         [ -- Universal quantification.
           -- [v1 v1 ... vn : T1]. T2
           do   pTok KSquareBra
                bs      <- P.many1 pBinder
                pTok (KOp ":")
                k       <- pTypeSum c
                pTok KSquareKet
                pTok KDot

                body    <- pTypeForall c

                return  $ foldr TForall body 
                        $ map (\b -> makeBindFromBinder b k) bs

           -- Body type
         , do   pTypeFun c]
 <?> "a type"


-- | Parse a function type.
pTypeFun 
        :: Ord n 
        => Context -> Parser n (Type n)

pTypeFun c
 = do   t1      <- pTypeApp c
        P.choice 
         [ -- T1 ~> T2
           do   pTok KArrowTilde
                t2      <- pTypeFun c
                return  $ TApp (TApp (TCon (TyConKind KiConFun)) t1) t2

           -- T1 => T2
         , do   pTok KArrowEquals
                t2      <- pTypeFun c
                return  $ TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2

           -- T1 -> T2
         , do   pTok KArrowDash
                t2      <- pTypeFun c
                if (  contextFunctionalEffects c
                   && contextFunctionalClosures c)
                   then return $ t1 `tFunPE` t2
                   else return $ t1 `tFun`   t2

           -- T1 -(TSUM | TSUM)> t2
         , do   pTok (KOp "-")
                pTok KRoundBra
                eff     <- pTypeSum c
                pTok (KOp "|")
                clo     <- pTypeSum c
                pTok KRoundKet
                pTok (KOp ">")
                t2      <- pTypeFun c
                return  $ tFunEC t1 eff clo t2


           -- Body type
         , do   return t1 ]
 <?> "an atomic type or type application"


-- | Parse a type application.
pTypeApp 
        :: Ord n 
        => Context -> Parser n (Type n)
pTypeApp c
 = do   (t:ts)  <- P.many1 (pTypeAtom c)
        return  $  foldl TApp t ts
 <?> "an atomic type or type application"


-- | Parse a variable, constructor or parenthesised type.
pTypeAtom 
        :: Ord n 
        => Context -> Parser n (Type n)
pTypeAtom c
 = P.choice
        -- (~>) and (=>) and (->) and (TYPE2)
        [ -- (~>)
          do    pTok (KOpVar "~>")
                return (TCon $ TyConKind KiConFun)

          -- (=>)
        , do    pTok (KOpVar "=>")
                return (TCon $ TyConWitness TwConImpl)

          -- (->)
        , do    pTok (KOpVar "->")

                -- Decide what type constructor to use for the (->) token.
                -- Only use the function constructor with latent effects
                -- and closures if the language fragment supports both.
                if (  contextFunctionalEffects  c 
                   && contextFunctionalClosures c)
                   then return (TCon $ TyConSpec TcConFunEC)
                   else return (TCon $ TyConSpec TcConFun)

        -- (TYPE2)
        , do    pTok KRoundBra
                t       <- pTypeSum c
                pTok KRoundKet
                return t 

        -- Named type constructors
        , do    so      <- pSoCon
                return  $ TCon (TyConSort so)

        , do    ki      <- pKiCon
                return  $ TCon (TyConKind ki)

        , do    tc      <- pTcCon
                return  $ TCon (TyConSpec tc)

        , do    tc      <- pTwCon
                return  $ TCon (TyConWitness tc)

        , do    tc      <- pTyConNamed
                return  $ TCon tc
            
        -- Bottoms.
        , do    pTokAs KBotEffect  (tBot kEffect)
        , do    pTokAs KBotClosure (tBot kClosure)
      
        -- Bound occurrence of a variable.
        --  We don't know the kind of this variable yet, so fill in the
        --  field with the bottom element of computation kinds. This isn't
        --  really part of the language, but makes sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v)

        , do    i       <- pIndex
                return  $  TVar (UIx (fromIntegral i))
        ]
 <?> "an atomic type"


-------------------------------------------------------------------------------
-- | Parse a builtin sort constructor.
pSoCon :: Parser n SoCon
pSoCon  =   P.pTokMaybe f
        <?> "a sort constructor"
 where f (KA (KSoConBuiltin c)) = Just c
       f _                      = Nothing 


-- | Parse a builtin kind constructor.
pKiCon :: Parser n KiCon
pKiCon  =   P.pTokMaybe f
        <?> "a kind constructor"
 where f (KA (KKiConBuiltin c)) = Just c
       f _                      = Nothing 


-- | Parse a builtin type constructor.
pTcCon :: Parser n TcCon
pTcCon  =   P.pTokMaybe f
        <?> "a type constructor"
 where f (KA (KTcConBuiltin c)) = Just c
       f _                      = Nothing 


-- | Parse a builtin witness type constructor.
pTwCon :: Parser n TwCon
pTwCon  =   P.pTokMaybe f
        <?> "a witness constructor"
 where f (KA (KTwConBuiltin c)) = Just c
       f _                      = Nothing


-- | Parse a user defined type constructor.
pTyConNamed :: Parser n (TyCon n)
pTyConNamed  
        =   P.pTokMaybe f
        <?> "a type constructor"
 where  f (KN (KCon n))          = Just (TyConBound (UName n) (tBot kData))
        f _                      = Nothing

