
-- | Core language parser.
module DDC.Core.Transform.Rewrite.Parser
        (pRule, pRuleMany)
where
import DDC.Core.Exp
import DDC.Core.Parser
import DDC.Core.Lexer.Tokens
import qualified DDC.Base.Parser                 as P
import qualified DDC.Type.Exp.Simple             as T
import qualified DDC.Core.Transform.Rewrite.Rule as R


-- Rewrite Rules ----------------------------------------------------------------
{-
    [r1 r2 : %] (x : Int r1).
        Const r1 =>
        addInt [:r1 r2 r1:] x (0 [r2] ()) =
        x
-}
-- | Parse a rewrite rule.
pRule   :: Ord n 
        => Context n -> Parser n (R.RewriteRule P.SourcePos n)
pRule c
 = do   bs       <- pRuleBinders c
        (cs,lhs) <- pRuleCsLhs c
        hole     <- pRuleHole c
        pSym SEquals
        rhs      <- pExp c

        return $ R.mkRewriteRule bs cs lhs hole rhs


{-
add_zero_r
    [r1 r2 : %] (x : Int r1).
        Const r1 =>
        addInt [:r1 r2 r1:] x (0 [r2] ()) =
        x;
add_zero_l
    [r1 r2 : %] ...
        ;
-}
-- | Parse many rewrite rules.
pRuleMany       
        :: Ord n 
        => Context n -> Parser n [(n,R.RewriteRule P.SourcePos n)]
pRuleMany c
 = P.many (do
        n <- pName
        r <- pRule c
        pSym SSemiColon
        return (n,r))


pRuleBinders 
        :: Ord n 
        => Context n -> Parser n [(R.BindMode,Bind n)]

pRuleBinders c
 = P.choice
 [ do   bs <- P.many1 (pBinders c)
        pSym SDot
        return $ concat bs
 , return []
 ]


pRuleCsLhs 
        :: Ord n 
        => Context n -> Parser n ([Type n], Exp P.SourcePos n)
pRuleCsLhs c
 = P.choice
 [ do   cs <- P.many1 $ P.try (do
                cc <- pTypeApp c
                pTok KArrowEquals
                return cc)
        lhs <- pExp c
        return (cs,lhs)
 , do   lhs <- pExp c
        return ([],lhs)
 ]


pRuleHole 
        :: Ord n 
        => Context n -> Parser n (Maybe (Exp P.SourcePos n))
pRuleHole c
 = P.optionMaybe
 $ do   pSym SUnderscore
        pSym SBraceBra
        e <- pExp c
        pSym SBraceKet
        pSym SUnderscore
        return e


-- | Parse rewrite binders
--
-- Many of:
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--
pBinders 
        :: Ord n 
        => Context n -> Parser n [(R.BindMode, Bind n)]
pBinders c
 = P.choice
 [ pBindersBetween c R.BMSpec      (pSym SSquareBra) (pSym SSquareKet)
 , pBindersBetween c (R.BMValue 0) (pSym SRoundBra)  (pSym SRoundKet)
 ]


pBindersBetween 
        :: Ord n 
        => Context n
        -> R.BindMode 
        -> Parser n a 
        -> Parser n a
        -> Parser n [(R.BindMode,Bind n)]

pBindersBetween c bm bra ket
 = do   bra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        ket
        return $ map (mk t) bs
 where mk t b = (bm,T.makeBindFromBinder b t)

