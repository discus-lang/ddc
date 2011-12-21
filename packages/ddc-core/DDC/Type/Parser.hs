
-- | Parser for type expressions.
module DDC.Type.Parser
        ( module DDC.Base.Parser
        , Parser
        , pType, pTypeAtom, pTypeApp
        , pBinder
        , pIndex
        , pTok, pTokAs)
where
import DDC.Type.Exp
import Control.Monad
import DDC.Type.Compounds
import DDC.Base.Parser                  ((<?>))
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Sum           as TS


-- HACKS: We're using the core tokens here so we don't have to duplicate
--        the parser so it works on Type tokens as well as Core tokens.
--        Perhaps we want a type class to recognise the tokens that are
--        common to both versions.
import DDC.Core.Parser.Tokens   


-- | Parser of type tokens.
type Parser n a
        = P.Parser (Tok n) a


-- | Top level parser for types.
pType   :: Ord n => Parser n (Type n)
pType   = pTypeSum


-- Sums
pTypeSum :: Ord n => Parser n (Type n)
pTypeSum 
 = do   t1      <- pTypeForall
        P.choice 
         [ -- Type sums.
           -- T2 + T3
           do   pTok KPlus
                t2      <- pTypeSum
                return  $ TSum $ TS.fromList (tBot sComp) [t1, t2]
                
         , do   return t1 ]

-- Binds
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


-- Foralls.
pTypeForall :: Ord n => Parser n (Type n)
pTypeForall
 = P.choice
         [ -- Universal quantification.
           -- [v1 v1 ... vn : T1]. T2
           do   pTok KSquareBra
                bs      <- P.many1 pBinder
                pTok KColon
                k       <- pTypeSum
                pTok KSquareKet
                pTok KDot

                body    <- pTypeForall

                return  $ foldr TForall body 
                        $ map (\b -> makeBindFromBinder b k) bs

           -- Body type
         , do   pTypeFun]


-- Functions
pTypeFun :: Ord n => Parser n (Type n)
pTypeFun
 = do   t1      <- pTypeApp
        P.choice 
         [ -- T1 -> T2
           do   pTok KTypeFun
                t2      <- pTypeFun
                return  $ t1 ->> t2

           -- T1 -(TSUM | TSUM)> t2
         , do   pTok KTypeFunBra
                eff     <- pTypeSum
                pTok KBar
                clo     <- pTypeSum
                pTok KTypeFunKet
                t2      <- pTypeFun
                return  $ tFun t1 eff clo t2

           -- T1 ~> T2
         , do   pTok KKindFun
                t2      <- pTypeFun
                return  $ TApp (TApp (TCon (TyConKind KiConFun)) t1) t2

           -- Body type
         , do   return t1 ]


-- Applications
pTypeApp :: Ord n => Parser n (Type n)
pTypeApp  
 = do   (t:ts)  <- P.many1 pTypeAtom
        return  $  foldl TApp t ts


-- Atomics
pTypeAtom :: Ord n => Parser n (Type n)
pTypeAtom  
 = P.choice
        -- (TYPE2) and (->)
        [ do    pTok KRoundBra
                P.choice
                 [ do   t       <- pTypeSum
                        pTok KRoundKet
                        return t 

                 , do   pTok KTypeFun
                        pTok KRoundKet
                        return (TCon $ TyConComp TcConFun)
                 ]

        -- Named type constructors
        , do    tc      <- pTcCon
                return  $ TCon (TyConComp tc)

        , do    tc      <- pTwCon
                return  $ TCon (TyConWitness tc)

        , do    tc      <- pTyConNamed
                return  $ TCon tc

        -- Symbolic constructors.
        , do    pTokAs KSortComp    (TCon $ TyConSort SoConComp)
        , do    pTokAs KSortProp    (TCon $ TyConSort SoConProp) 
        , do    pTokAs KKindValue   (TCon $ TyConKind KiConData)
        , do    pTokAs KKindRegion  (TCon $ TyConKind KiConRegion) 
        , do    pTokAs KKindEffect  (TCon $ TyConKind KiConEffect) 
        , do    pTokAs KKindClosure (TCon $ TyConKind KiConClosure) 
        , do    pTokAs KKindWitness (TCon $ TyConKind KiConWitness) 
            
        -- Bottoms.
        , do    pTokAs KBotEffect  (tBot kEffect)
        , do    pTokAs KBotClosure (tBot kClosure)
      
        -- Bound occurrence of a variable.
        --  We don't know the kind of this variable yet, so fill in the
        --  field with the bottom element of computation kinds. This isn't
        --  really part of the language, but makes sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v (tBot sComp))

        , do    i       <- pIndex
                return  $  TVar (UIx (fromIntegral i) (tBot sComp))
        ]
 <?> "atomic type"


-------------------------------------------------------------------------------
-- | Parse a builtin `TcCon`
pTcCon :: Parser n TcCon
pTcCon  = P.pTokMaybe f
 where f (KA (KTcConBuiltin c)) = Just c
       f _                      = Nothing 

-- | Parse a builtin `TwCon`
pTwCon :: Parser n TwCon
pTwCon  = P.pTokMaybe f
 where f (KA (KTwConBuiltin c)) = Just c
       f _                      = Nothing

-- | Parse a user `TcCon`
pTyConNamed :: Parser n (TyCon n)
pTyConNamed  = P.pTokMaybe f
 where  f (KN (KCon n))          = Just (TyConBound (UName n (tBot kData)))
        f _                      = Nothing

-- | Parse a variable.
pVar :: Parser n n
pVar    = P.pTokMaybe f
 where  f (KN (KVar n))         = Just n
        f _                     = Nothing

-- | Parse a debuijn index
pIndex :: Parser n Int
pIndex = P.pTokMaybe f
 where  f (KA (KIndex i))       = Just i
        f _                     = Nothing

-- | Parse an atomic token.
pTok :: TokAtom -> Parser n ()
pTok k     = P.pTok (KA k)


-- | Parse an atomic token and return some value.
pTokAs :: TokAtom -> a -> Parser n a
pTokAs k x = P.pTokAs (KA k) x


