
-- | Parser for type expressions.
module DDC.Type.Parser
        ( module DDC.Base.Parser
        , Parser
        , pType)
where
import DDC.Type.Exp
import DDC.Type.Parser.Tokens
import DDC.Base.Parser
import Control.Monad
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Sum           as TS


-- | Parser of type tokens.
type Parser k n a
        = ParserG (Tokens k n) k n a


-- | Top level parser for types.
pType   :: Ord n => Parser k n (Type n)
pType   = pType4


-- Foralls.
pType4 :: Ord n => Parser k n (Type n)
pType4
 = do   choice
         [ -- Universal quantification.
           -- [v11 v12 ... v1n : T2, v21 v22 ... v2n : T2]. T3
           do   pTok tSquareBra
                vsk     <- sepBy1 
                            (do vs      <- many1 pVar 
                                pTok tColon
                                k       <- pType2
                                return  $ (vs, k))
                            (pTok tComma)
                pTok tSquareKet
                pTok tDot

                body    <- pType3

                return  $ foldr TForall body 
                        $ [ BName v k   | (vs, k) <- vsk
                                        , v       <- vs]
          
           -- Body type
         , do   pType2]

-- Sums
pType3 :: Ord n => Parser k n (Type n)
pType3 
 = do   t1      <- pType2
        choice 
         [ -- Type sums.
           -- T2 + T3
           do   pTok tPlus
                t2      <- pType3
                return  $ TSum $ TS.fromList (TBot T.sComp) [t1, t2]
                
         , do   return t1 ]


-- Functions
pType2 :: Ord n => Parser k n (Type n)
pType2
 = do   t1      <- pType1
        choice 
         [ -- T1 -> T2
           do   pTok tTypeFun
                t2      <- pType2
                return  $ t1 T.->> t2

           -- T1 -(T0 T0)> t2
         , do   pTok tTypeFunBra
                eff     <- pType0
                clo     <- pType0
                pTok tTypeFunKet
                t2      <- pType2
                return  $ T.tFun t1 t2 eff clo

           -- T1 ~> T2
         , do   pTok tKindFun
                t2      <- pType2
                return  $ TApp (TApp (TCon (TyConKind KiConFun)) t1) t2

           -- Body type
         , do   return t1 ]


-- Applications
pType1 :: Ord n => Parser k n (Type n)
pType1  
 = do   (t:ts)  <- many1 pType0
        return  $  foldl TApp t ts


-- Atomics
pType0 :: Ord n => Parser k n (Type n)
pType0  = choice
        -- (TYPE2) and (->)
        [ do    pTok tRoundBra
                choice
                 [ do   t       <- pType2
                        pTok tRoundKet
                        return t 

                 , do   pTok tTypeFun
                        pTok tRoundKet
                        return (TCon $ TyConComp TcConFun)
                 ]

        -- Named type constructors
        , do    tc      <- pTwConBuiltin
                return  $ TCon (TyConWitness tc)

        , do    tc      <- pTcConBuiltin
                return  $ TCon (TyConComp tc)

        , do    tc      <- pTcConData
                return  $ TCon (TyConComp tc)

        -- Symbolic constructors.
        , do    pTokenAs tSortComp    (TCon $ TyConSort SoConComp)
        , do    pTokenAs tSortProp    (TCon $ TyConSort SoConProp) 
        , do    pTokenAs tKindValue   (TCon $ TyConKind KiConData)
        , do    pTokenAs tKindRegion  (TCon $ TyConKind KiConRegion) 
        , do    pTokenAs tKindEffect  (TCon $ TyConKind KiConEffect) 
        , do    pTokenAs tKindClosure (TCon $ TyConKind KiConClosure) 
        , do    pTokenAs tKindWitness (TCon $ TyConKind KiConWitness) 
            
        -- Bottoms.
        , do    pTokenAs tBotEffect  (TBot T.kEffect)
        , do    pTokenAs tBotClosure (TBot T.kClosure)
      
        -- Bound occurrence of a variable.
        -- We don't know the kind of this variable yet, so fill in the field with the bottom
        -- element of computation kinds. This isn't really part of the language, but makes
        -- sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v (TBot T.sComp))
        ]


---------------------------------------------------------------------------------------------------
-- | Parse a builtin named `TwCon`
pTwConBuiltin :: Parser k n TwCon
pTwConBuiltin   = pToken tTwConBuiltin

-- | Parse a builtin named `TcCon`
pTcConBuiltin :: Parser k n (TcCon n)
pTcConBuiltin   = pToken tTcConBuiltin

-- | Parse a user defined named tycon.
pTcConData :: Parser k n (TcCon n)
pTcConData      = pToken tTcConData

-- | Parse a variable.
pVar :: Parser k n n
pVar            = pToken tVar

