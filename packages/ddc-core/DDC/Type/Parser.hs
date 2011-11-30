
-- | Parser for type expressions.
module DDC.Type.Parser
        ( module DDC.Base.Parser
        , Parser
        , pType)
where
import DDC.Type.Exp
import DDC.Type.Parser.Tokens
import Control.Monad
import DDC.Base.Parser                  (pTokMaybe, pTokAs, pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Sum           as TS


-- | Parser of type tokens.
type Parser n a
        = P.Parser (Tok n) a


-- | Top level parser for types.
pType   :: Ord n => Parser n (Type n)
pType   = pType4


-- Foralls.
pType4 :: Ord n => Parser n (Type n)
pType4
 = do   P.choice
         [ -- Universal quantification.
           -- [v11 v12 ... v1n : T2, v21 v22 ... v2n : T2]. T3
           do   pTok KSquareBra
                vsk     <- P.sepBy1 
                            (do vs      <- P.many1 pVar 
                                pTok KColon
                                k       <- pType2
                                return  $ (vs, k))
                            (pTok KComma)
                pTok KSquareKet
                pTok KDot

                body    <- pType3

                return  $ foldr TForall body 
                        $ [ BName v k   | (vs, k) <- vsk
                                        , v       <- vs]

           -- Body type
         , do   pType2]

-- Sums
pType3 :: Ord n => Parser n (Type n)
pType3 
 = do   t1      <- pType2
        P.choice 
         [ -- Type sums.
           -- T2 + T3
           do   pTok KPlus
                t2      <- pType3
                return  $ TSum $ TS.fromList (TBot T.sComp) [t1, t2]
                
         , do   return t1 ]


-- Functions
pType2 :: Ord n => Parser n (Type n)
pType2
 = do   t1      <- pType1
        P.choice 
         [ -- T1 -> T2
           do   pTok KTypeFun
                t2      <- pType2
                return  $ t1 T.->> t2

           -- T1 -(T0 T0)> t2
         , do   pTok KTypeFunBra
                eff     <- pType0
                clo     <- pType0
                pTok KTypeFunKet
                t2      <- pType2
                return  $ T.tFun t1 t2 eff clo

           -- T1 ~> T2
         , do   pTok KKindFun
                t2      <- pType2
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
                 [ do   t       <- pType2
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
        -- We don't know the kind of this variable yet, so fill in the field with the bottom
        -- element of computation kinds. This isn't really part of the language, but makes
        -- sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v (TBot T.sComp))
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

