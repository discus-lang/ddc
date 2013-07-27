
-- | Core language parser.
module DDC.Core.Transform.Rewrite.Parser
        (pRule, pRuleMany)
where
import DDC.Core.Exp
import DDC.Core.Parser
import DDC.Core.Lexer.Tokens
import qualified DDC.Base.Parser                 as P
import qualified DDC.Type.Compounds              as T
import qualified DDC.Core.Transform.Rewrite.Rule as R


-- Rewrite Rules ----------------------------------------------------------------
{-
    [r1 r2 : %] (x : Int r1).
	Const r1 =>
	addInt [:r1 r2 r1:] x (0 [r2] ()) =
	x
-}
-- | Parse a rewrite rule.
pRule	:: Ord n 
        => Context -> Parser n (R.RewriteRule P.SourcePos n)
pRule c
 = do	bs	 <- pRuleBinders c
	(cs,lhs) <- pRuleCsLhs c
	hole	 <- pRuleHole c
	pTok (KOp "=")
	rhs	 <- pExp c

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
        => Context -> Parser n [(n,R.RewriteRule P.SourcePos n)]
pRuleMany c
 = P.many (do
        n <- pName
        r <- pRule c
        pTok KSemiColon
        return (n,r))


pRuleBinders 
        :: Ord n 
        => Context -> Parser n [(R.BindMode,Bind n)]

pRuleBinders c
 = P.choice
 [ do	bs <- P.many1 (pBinders c)
	pTok KDot
	return $ concat bs
 , return []
 ]


pRuleCsLhs 
        :: Ord n 
        => Context -> Parser n ([Type n], Exp P.SourcePos n)
pRuleCsLhs c
 = P.choice
 [ do	cs <- P.many1 $ P.try (do
		cc <- pTypeApp c
		pTok KArrowEquals
		return cc)
	lhs <- pExp c
	return (cs,lhs)
 , do	lhs <- pExp c
	return ([],lhs)
 ]


pRuleHole 
        :: Ord n 
        => Context -> Parser n (Maybe (Exp P.SourcePos n))
pRuleHole c
 = P.optionMaybe
 $ do	pTok KBraceBra
	e <- pExp c
	pTok KBraceKet
	return e


-- | Parse rewrite binders
--
-- Many of:
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--
pBinders 
        :: Ord n 
        => Context -> Parser n [(R.BindMode, Bind n)]
pBinders c
 = P.choice
 [ pBindersBetween c R.BMSpec      (pTok KSquareBra) (pTok KSquareKet)
 , pBindersBetween c (R.BMValue 0) (pTok KRoundBra)  (pTok KRoundKet)
 ]


pBindersBetween 
        :: Ord n 
        => Context
        -> R.BindMode 
        -> Parser n () 
        -> Parser n () 
        -> Parser n [(R.BindMode,Bind n)]

pBindersBetween c bm bra ket
 = do	bra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        ket
        return $ map (mk t) bs
 where mk t b = (bm,T.makeBindFromBinder b t)

