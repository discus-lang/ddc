{-# LANGUAGE TypeFamilies #-}

-- | Parser for Source Tetra expressions.
module DDC.Source.Tetra.Parser.Exp
        ( pExp
        , pExpApp
        , pExpAtom,     pExpAtomSP
        , pLetsSP,      pClauseSP
        , pType
        , pTypeApp
        , pTypeAtomSP)
where
import DDC.Source.Tetra.Transform.Guards
import DDC.Source.Tetra.Parser.Type
import DDC.Source.Tetra.Parser.Witness
import DDC.Source.Tetra.Parser.Param
import DDC.Source.Tetra.Parser.Base
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim            as S
import DDC.Core.Lexer.Tokens
import Control.Monad.Except
import qualified DDC.Base.Parser        as P
import qualified Data.Text              as Text


type SP = SourcePos


-- Exp --------------------------------------------------------------------------------------------
-- | Parse a Tetra Source language expression.
pExp :: Parser Exp
pExp
 = P.choice

        -- Level-0 lambda abstractions
        --  \(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
        --  \x1 x2 : Type. Exp
 [ do   sp      <- P.choice [ pTokSP KLambda, pTokSP KBackSlash ]

        bs      <- P.choice
                [ fmap concat $ P.many1 
                   $ do pTok KRoundBra
                        bs'     <- P.many1 pBind
                        pTok (KOp ":")
                        t       <- pType
                        pTok KRoundKet
                        return  [ XBindVarMT b (Just t)
                                | b <- bs']

                , do    bs'     <- P.many1 pBind
                        pTok (KOp ":")
                        t       <- pType
                        return  [ XBindVarMT b (Just t)
                                | b <- bs']
                ]

        pTok KDot
        xBody   <- pExp
        return  $ XAnnot sp $ foldr XLam xBody bs

        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
 , do   sp      <- P.choice [ pTokSP KBigLambda, pTokSP KBigLambdaSlash ]

        bs      <- P.choice
                [ fmap concat $ P.many1
                   $ do pTok KRoundBra
                        bs'     <- P.many1 pBind
                        pTok (KOp ":")
                        t       <- pType
                        pTok KRoundKet
                        return  $ map (\b -> XBindVarMT b (Just t)) bs'

                , do    bs'     <- P.many1 pBind
                        pTok (KOp ":")
                        t       <- pType
                        return  $ map (\b -> XBindVarMT b (Just t)) bs'
                ]

        pTok KDot
        xBody   <- pExp
        return  $ XAnnot sp $ foldr XLAM xBody bs

        -- let expression
 , do   (lts, sp) <- pLetsSP
        pTok    KIn
        x2      <- pExp
        return  $ XAnnot sp $ XLet lts x2

        -- Sugar for a let-expression.
        --  do { Stmt;+ }
 , do   pTok    KDo
        pTok    KBraceBra
        xx      <- pStmts
        pTok    KBraceKet
        return  $ xx

        -- case Exp of { Alt;+ }
 , do   sp      <- pTokSP KCase
        x       <- pExp
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 pAltCase (pTok KSemiColon)
        pTok KBraceKet
        return  $ XAnnot sp $ XCase x alts

        -- match { | EXP = EXP | EXP = EXP ... }
        --  Sugar for cascaded case expressions case-expression.
 , do   sp      <- pTokSP KMatch
        pTok KBraceBra

        gxs     <- liftM (map (AAltMatch . snd))
                $  P.sepEndBy1  (pGuardedExpSP (pTokSP KEquals)) 
                                (pTok KSemiColon)

        let xError
                = makeXErrorDefault 
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)

        -- Desugar guards.
        -- If none match then raise a runtime error.
{-
        let xx' = desugarGuards gg
                $ makeXErrorDefault
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)
-}
        pTok KBraceKet
        return  $ XAnnot sp $ XMatch sp gxs xError

 , do   -- if-then-else
        --  Sugar for a case-expression.
        sp      <- pTokSP KIf
        x1      <- pExp
        pTok KThen
        x2      <- pExp
        pTok KElse
        x3      <- pExp 
        return  $ XAnnot sp $ XCase x1 
                        [ AAltCase PTrue    [GExp x2]
                        , AAltCase PDefault [GExp x3]]

        -- weakeff [Type] in Exp
 , do   sp      <- pTokSP KWeakEff
        pTok KSquareBra
        t       <- pType
        pTok KSquareKet
        pTok KIn
        x       <- pExp
        return  $ XAnnot sp $ XCast (CastWeakenEffect t) x

        -- purify Witness in Exp
 , do   sp      <- pTokSP KPurify
        w       <- pWitness
        pTok KIn
        x       <- pExp
        return  $ XAnnot sp $ XCast (CastPurify w) x

        -- box Exp
 , do   sp      <- pTokSP KBox
        x       <- pExp
        return  $ XAnnot sp $ XCast CastBox x

        -- run Exp
 , do   sp      <- pTokSP KRun
        x       <- pExp
        return  $ XAnnot sp $ XCast CastRun x

        -- APP
 , do   pExpApp
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Parser Exp
pExpApp
  = do  xps     <- liftM concat $ P.many1 pArgSPs
        let (xs, sps) = unzip xps
        let (sp1 : _) = sps
                
        case xs of
         [x]    -> return x
         _      -> return $ XDefix sp1 xs

  <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: Parser [(Exp, SP)]
pArgSPs 
 = P.choice
        -- [Type]
 [ do   sp      <- pTokSP KSquareBra
        t       <- pType
        pTok KSquareKet
        return  [(XType t, sp)]

        -- [: Type0 Type0 ... :]
 , do   sp      <- pTokSP KSquareColonBra
        ts      <- fmap (fst . unzip) $ P.many1 pTypeAtomSP
        pTok KSquareColonKet
        return  [(XType t, sp) | t <- ts]
        
        -- { Witness }
 , do   sp      <- pTokSP KBraceBra
        w       <- pWitness
        pTok KBraceKet
        return  [(XWitness w, sp)]
                
        -- {: Witness0 Witness0 ... :}
 , do   sp      <- pTokSP KBraceColonBra
        ws      <- P.many1 pWitnessAtom
        pTok KBraceColonKet
        return  [(XWitness w, sp) | w <- ws]
               
        -- Exp0
 , do   (x, sp)  <- pExpAtomSP
        return  [(x, sp)]
 ]
 <?> "a type, witness or expression argument"


-- | Parse a variable, constructor or parenthesised expression.
pExpAtom :: Parser Exp
pExpAtom
 = do   (x, _) <- pExpAtomSP
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP :: Parser (Exp, SP)
pExpAtomSP
 = P.choice
 [      -- ( Exp2 )
   do   sp      <- pTokSP KRoundBra
        t       <- pExp
        pTok KRoundKet
        return  (t, sp)

        -- Infix operator used as a variable.
 , do   (UName tx, sp) <- pBoundNameOpVarSP
        return  (XInfixVar sp (Text.unpack tx), sp)

        -- Infix operator used nekkid.
 , do   (UName tx, sp) <- pBoundNameOpSP
        return  (XInfixOp  sp (Text.unpack tx), sp)
  
        -- The unit data constructor.       
 , do   sp              <- pTokSP KDaConUnit
        return  (XCon  dcUnit, sp)

        -- Named algebraic constructors.
 , do   (con, sp)       <- pDaConBoundNameSP
        return  (XCon  (DaConBound con), sp)

        -- Literals.
        --  We just fill-in the type with a hole for now, and leave it to
        --  We also set the literal as being algebraic, which may not be
        --  true (as for Floats). The spreader also needs to fix this.
 , do   (lit, sp)       <- pDaConBoundLitSP
        return  (XCon  (DaConPrim lit (TVar UHole)), sp)

        -- Primitive names.
 , do   (nPrim, sp)     <- pPrimValSP
        return  (XPrim nPrim, sp)

        -- Named variables.
 , do   (u,  sp)        <- pBoundNameSP
        return  (XVar u, sp)

        -- Debruijn indices
 , do   (u, sp)         <- pBoundIxSP
        return  (XVar u, sp)

 ]

 <?> "a variable, constructor, or parenthesised type"


-- Alternatives -----------------------------------------------------------------------------------
-- Case alternatives.
pAltCase :: Parser AltCase
pAltCase
 = do   p       <- pPat
        P.choice
         [ do   -- Desugar case guards while we're here.
                spgxs     <- P.many1 (pGuardedExpSP (pTokSP KArrowDash))
                let gxs  = map snd spgxs
                return  $ AAltCase p gxs 
                
         , do   pTok KArrowDash
                x       <- pExp
                return  $ AAltCase p [GExp x] ]


-- Patterns.
pPat :: Parser Pat
pPat
 = P.choice
 [      -- Wildcard
   do   pTok KUnderscore
        return  $ PDefault

        -- Lit
 , do   nLit    <- pDaConBoundLit
        return  $ PData (DaConPrim nLit (TBot S.KData)) []

        -- 'Unit'
 , do   pTok KDaConUnit
        return  $ PData  dcUnit []

        -- Con Bind Bind ...
 , do   nCon    <- pDaConBoundName 
        bs      <- P.many pBindPat
        return  $ PData (DaConBound nCon) bs]


-- Binds in patterns can have no type annotation.
pBindPat :: Parser Bind
pBindPat 
 = P.choice
        -- Plain binder.
 [ do   b       <- pBind
        return  b
 ]


-- Guards -----------------------------------------------------------------------------------------
-- | Parse some guards and auto-desugar them to a case-expression.
pBindGuardsAsCaseSP :: Parser (SP, Exp)
pBindGuardsAsCaseSP
 = do   
        (sp, g) : spgs  
                <- P.many1 (pGuardedExpSP (pTokSP KEquals))

        -- Desugar guards.
        -- If none match then raise a runtime error.
        let xx' = desugarGuards (g : map snd spgs)  
                $ makeXErrorDefault 
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)

        return  (sp, xx')


-- | An guarded expression,
--   like | EXP1 = EXP2.
pGuardedExpSP 
        :: Parser  SP   -- ^ Parser for char between and of guards and exp.
                        --   usually -> or =
        -> Parser  (SP, GuardedExp)

pGuardedExpSP pTermSP
 = pGuardExp (pTokSP KBar)

 where  pGuardExp pSepSP
         = P.choice
         [ do   sp      <- pSepSP
                g       <- pGuard
                gx      <- liftM snd $ pGuardExp (pTokSP KComma)
                return  (sp, GGuard g gx)

         , do   sp      <- pTermSP
                x       <- pExp
                return  (sp, GExp x) ]

        pGuard
         = P.choice 
         [ P.try $
           do   p       <- pPat
                pTok KArrowDashLeft
                x       <- pExp
                return $ GPat p x

         , do   g       <- pExp
                return $ GPred g

         , do   pTok KOtherwise
                return GDefault ]


-- Bindings ---------------------------------------------------------------------------------------
pLetsSP :: Parser (Lets, SP)
pLetsSP 
 = P.choice
    [ -- non-recursive let
      do sp       <- pTokSP KLet
         l        <- liftM fst $ pClauseSP
         return (LGroup [l], sp)

      -- recursive let
    , do sp       <- pTokSP KLetRec
         pTok KBraceBra
         ls       <- liftM (map fst)
                  $  P.sepEndBy1 pClauseSP (pTok KSemiColon)
         pTok KBraceKet
         return (LGroup ls, sp)

      -- Private region binding.
      --   private Binder+ (with { Binder : Type ... })? in Exp
    , do sp     <- pTokSP KPrivate
         
        -- new private region names.
         bs     <- P.manyTill pBind
                $  P.try $ P.lookAhead $ P.choice [pTok KIn, pTok KWith]
         
         -- Witness types.
         r      <- pLetWits bs Nothing
         return (r, sp)

      -- Extend an existing region.
      --   extend Binder+ using Type (with { Binder : Type ...})? in Exp
    , do sp     <- pTokSP KExtend

         -- parent region
         t      <- pType
         pTok KUsing

         -- new private region names.
         bs     <- P.manyTill pBind
                $  P.try $ P.lookAhead 
                         $ P.choice [pTok KUsing, pTok KWith, pTok KIn]
         
         -- witness types
         r      <- pLetWits bs (Just t)
         return (r, sp)
    ]
    
    
pLetWits :: [Bind] -> Maybe Type -> Parser Lets
pLetWits bs mParent
 = P.choice 
    [ do   pTok KWith
           pTok KBraceBra
           wits    <- P.sepBy (P.choice
                      [ -- Named witness binder.
                        do b    <- pBind
                           pTok (KOp ":")
                           t    <- pTypeApp
                           return (b, t)

                        -- Ambient witness binding, used for capabilities.
                      , do t    <- pTypeApp
                           return (BNone, t)
                      ])
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LPrivate bs mParent wits)
    
    , do   return (LPrivate bs mParent [])
    ]


-- | A binding for let expression.
pClauseSP :: Parser (Clause, SP)
pClauseSP
 = do   b       <- pBind

        P.choice
         [ do   -- Binding with full type signature.
                sp         <- pTokSP (KOp ":")
                t          <- pType
                (_, xBody) <- pBindGuardsAsCaseSP
                return  ( SLet sp (XBindVarMT b (Just t)) [] [GExp xBody]
                        , sp)

         , do   -- Non-function binding with no type signature.
                sp      <- pTokSP KEquals
                xBody   <- pExp
                return  ( SLet sp (XBindVarMT b Nothing) [] [GExp xBody]
                        , sp)

         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many pBindParamSpec
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   Binder Param1 Param2 .. ParamN : Type = Exp
                        pTok (KOp ":")
                        tBody       <- pType
                        (sp, xBody) <- pBindGuardsAsCaseSP

                        let x   = expOfParams ps xBody
                        let t   = funTypeOfParams ps tBody
                        return  ( SLet sp (XBindVarMT b (Just t)) [] [GExp x]
                                , sp)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --   Binder Param1 Param2 .. ParamN = Exp
                 , do   (sp, xBody) <- pBindGuardsAsCaseSP

                        let x   = expOfParams ps xBody
                        return  ( SLet sp (XBindVarMT b Nothing) [] [GExp x]
                                , sp)
                 ]
         ]


-- Statements -------------------------------------------------------------------------------------
data Stmt
        = StmtBind  SP (GXBindVarMT Source) Exp
        | StmtMatch SP Pat Exp Exp
        | StmtNone  SP Exp


-- | Parse a single statement.
pStmt :: Parser Stmt
pStmt
 = P.choice
 [ -- Binder = Exp ;
   -- We need the 'try' because a VARIABLE binders can also be parsed
   --   as a function name in a non-binding statement.
   --  
   P.try $ 
    do  b       <- pBind
        sp      <- pTokSP KEquals
        x1      <- pExp
        return  $ StmtBind sp (XBindVarMT b Nothing) x1

   -- Pat <- Exp else Exp ;
   -- Sugar for a case-expression.
   -- We need the 'try' because the PAT can also be parsed
   --  as a function name in a non-binding statement.
 , P.try $
    do  p       <- pPat
        sp      <- pTokSP KArrowDashLeft
        x1      <- pExp
        pTok KElse
        x2      <- pExp
        return  $ StmtMatch sp p x1 x2

        -- Exp
 , do   x       <- pExp

        -- This should always succeed because pExp doesn't
        -- parse plain types or witnesses
        let Just sp     = takeAnnotOfExp x
        
        return  $ StmtNone sp x
 ]


-- | Parse some statements.
pStmts :: Parser Exp
pStmts 
 = do   stmts   <- P.sepEndBy1 pStmt (pTok KSemiColon)
        case makeStmts stmts of
         Nothing -> P.unexpected "do-block must end with a statement"
         Just x  -> return x


-- | Make an expression from some statements.
makeStmts :: [Stmt] -> Maybe Exp
makeStmts ss
 = case ss of
        [StmtNone _ x]    
         -> Just x

        StmtNone sp x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XAnnot sp $ XLet (LLet (XBindVarMT BNone Nothing) x1) x2

        StmtBind sp b x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XAnnot sp $ XLet (LLet b x1) x2

        StmtMatch sp p x1 x2 : rest
         | Just x3      <- makeStmts rest
         -> Just $ XAnnot sp $ XCase x1 
                 [ AAltCase p        [GExp x3]
                 , AAltCase PDefault [GExp x2] ]

        _ -> Nothing

