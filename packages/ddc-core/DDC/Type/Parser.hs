
-- | Parser for type expressions.
module DDC.Type.Parser
        ( module DDC.Base.Parser
        , Parser
        , pType
        , pInteger)
where
import DDC.Type.Exp
import Control.Monad
import DDC.Base.Parser                  (pTokMaybe, pTokAs, pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
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
                return  $ TSum $ TS.fromList (TBot T.sComp) [t1, t2]
                
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
                        $ map (\b -> T.makeBindFromBinder b k) bs

           -- Body type
         , do   pTypeFun]


-- Functions
pTypeFun :: Ord n => Parser n (Type n)
pTypeFun
 = do   t1      <- pType1
        P.choice 
         [ -- T1 -> T2
           do   pTok KTypeFun
                t2      <- pTypeFun
                return  $ t1 T.->> t2

           -- T1 -(TSUM | TSUM)> t2
         , do   pTok KTypeFunBra
                eff     <- pTypeSum
                pTok KBar
                clo     <- pTypeSum
                pTok KTypeFunKet
                t2      <- pTypeFun
                return  $ T.tFun t1 eff clo t2

           -- T1 ~> T2
         , do   pTok KKindFun
                t2      <- pTypeFun
                return  $ TApp (TApp (TCon (TyConKind KiConFun)) t1) t2

           -- Body type
         , do   return t1 ]


-- Applications
pType1 :: Ord n => Parser n (Type n)
pType1  
 = do   (t:ts)  <- P.many1 pType0
        return  $  foldl TApp t ts


-- Atomics
pType0 :: Ord n => Parser n (Type n)
pType0  = P.choice
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
        , do    tc      <- pTwCon
                return  $ TCon (TyConWitness tc)

        , do    tc      <- pTcConNamed
                return  $ TCon (TyConComp tc)

        -- Symbolic constructors.
        , do    pTokAs KSortComp    (TCon $ TyConSort SoConComp)
        , do    pTokAs KSortProp    (TCon $ TyConSort SoConProp) 
        , do    pTokAs KKindValue   (TCon $ TyConKind KiConData)
        , do    pTokAs KKindRegion  (TCon $ TyConKind KiConRegion) 
        , do    pTokAs KKindEffect  (TCon $ TyConKind KiConEffect) 
        , do    pTokAs KKindClosure (TCon $ TyConKind KiConClosure) 
        , do    pTokAs KKindWitness (TCon $ TyConKind KiConWitness) 
            
        -- Bottoms.
        , do    pTokAs KBotEffect  (TBot T.kEffect)
        , do    pTokAs KBotClosure (TBot T.kClosure)
      
        -- Bound occurrence of a variable.
        --  We don't know the kind of this variable yet, so fill in the field with the bottom
        --  element of computation kinds. This isn't really part of the language, but makes
        --  sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v (TBot T.sComp))

        , do    pTok KHat
                i       <- pInteger
                return  $  TVar (UIx (fromIntegral i) (TBot T.sComp))
        ]


---------------------------------------------------------------------------------------------------
-- | Parse a named `TwCon`
pTwCon :: Parser n TwCon
pTwCon  = pTokMaybe 
        $ \k -> case k of
                 KTwConBuiltin c  -> Just c
                 _                -> Nothing

-- | Parse a named `TcCon`
pTcConNamed :: Parser n (TcCon n)
pTcConNamed 
        = pTokMaybe
        $ \k -> case k of
                 KTcConBuiltin c  -> Just c
                 KCon n           -> Just (TcConData n (T.tBot T.kData))
                 _                -> Nothing

-- | Parse a variable.
pVar :: Parser n n
pVar    = pTokMaybe
        $ \k -> case k of
                 KVar n         -> Just n
                 _              -> Nothing

-- | Parse an integer.
pInteger :: Parser n Integer
pInteger
        = pTokMaybe
        $ \k -> case k of
                 KInteger i     -> Just i
                 _              -> Nothing
