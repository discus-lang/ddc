
-- | Core language parser.
module DDC.Core.Transform.Rewrite.Parser
        (pRule)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Core.Parser
import DDC.Type.Parser                  (pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Parser        as T
import qualified DDC.Core.Transform.Rewrite.Rule  as R


-- Rewrite Rules ----------------------------------------------------------------
{-
    [r1 r2 : %] (x : Int r1).
	Const r1 =>
	addInt [:r1 r2 r1:] x (0 [r2] ()) =
	x
-}
pRule	:: Ord n => Parser n (R.RewriteRule () n)
pRule
 = do	bs  <- pRuleBinders
	(cs,lhs) <- pRuleCsLhs
	pTok KEquals
	rhs <- pExp

	return $ R.mkRewriteRule bs cs lhs rhs

pRuleBinders :: Ord n => Parser n [(R.BindMode,Bind n)]
pRuleBinders
 = P.choice
 [ do	bs <- P.many1 pBinders
	pTok KDot
	return $ concat bs
 , return []
 ]

pRuleCsLhs :: Ord n => Parser n ([Type n], Exp () n)
pRuleCsLhs
 = P.choice
 [ do	cs <- P.many1 $ P.try (do
		c <- T.pTypeApp
		pTok KArrowEquals
		return c)
	lhs <- pExp
	return (cs,lhs)
 , do	lhs <- pExp
	return ([],lhs)
 ]

-- | Parse rewrite binders
--
-- Many of:
--       [BIND1 BIND2 .. BINDN : TYPE]
--   or  (BIND : TYPE)
--
pBinders :: Ord n => Parser n [(R.BindMode, Bind n)]
pBinders
 = P.choice
 [ pBindersBetween R.BMKind (pTok KSquareBra) (pTok KSquareKet)
 , pBindersBetween R.BMType (pTok KRoundBra) (pTok KRoundKet)
 ]

pBindersBetween :: Ord n => R.BindMode ->
		    Parser n () -> Parser n () ->
		    Parser n [(R.BindMode,Bind n)]
pBindersBetween bm bra ket
 = do	bra
        bs      <- P.many1 T.pBinder
        pTok KColon
        t       <- T.pType
        ket
        return $ map (mk t) bs
 where mk t b = (bm,T.makeBindFromBinder b t)
