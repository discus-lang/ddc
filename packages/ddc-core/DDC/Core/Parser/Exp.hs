
-- | Core language parser.
module DDC.Core.Parser.Exp
        ( pExp
        , pExpApp
        , pExpAtom,     pExpAtomSP
        , pLetsSP
        , pType
        , pTypeApp
        , pTypeAtom)
where
import DDC.Core.Exp
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Param
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Compounds
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import Control.Monad.Error


-- Expressions ----------------------------------------------------------------
-- | Parse a core language expression.
pExp    :: Ord n => Context -> Parser n (Exp SourcePos n)
pExp c
 = P.choice
        -- Level-0 lambda abstractions
        -- \BIND.. . EXP
 [ do   sp      <- pTokSP KBackSlash
        bs      <- liftM concat $ P.many1 (pBinds c)
        pTok KDot
        xBody   <- pExp c
        return  $ foldr (XLam sp) xBody bs


        -- Level-1 lambda abstractions.
        -- /\BINDS.. . EXP
 , do   sp      <- pTokSP KBigLambda
        bs      <- liftM concat $ P.many1 (pBinds c)
        pTok KDot
        xBody   <- pExp c
        return  $ foldr (XLAM sp) xBody bs


        -- let expression
 , do   (lts, sp) <- pLetsSP c
        pTok    KIn
        x2      <- pExp c
        return  $ XLet sp lts x2


        -- do { STMTS }
        --   Sugar for a let-expression.
 , do   pTok    KDo
        pTok    KBraceBra
        xx      <- pStmts c
        pTok    KBraceKet
        return  $ xx


        -- withregion CON in EXP
 , do   sp      <- pTokSP KWithRegion
        u       <- P.choice 
                [  do   n    <- pVar
                        return $ UName n

                ,  do   n    <- pCon
                        return $ UPrim n kRegion]
        pTok KIn
        x       <- pExp c
        return  $ XLet sp (LWithRegion u) x


        -- case EXP of { ALTS }
 , do   sp      <- pTokSP KCase
        x       <- pExp c
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 (pAlt c) (pTok KSemiColon)
        pTok KBraceKet
        return  $ XCase sp x alts


        -- letcase PAT = EXP in EXP
 , do   --  Sugar for a single-alternative case expression.
        sp      <- pTokSP KLetCase
        p       <- pPat c
        pTok (KOp "=")
        x1      <- pExp c
        pTok KIn
        x2      <- pExp c
        return  $ XCase sp x1 [AAlt p x2]


        -- match PAT <- EXP else EXP in EXP
        --  Sugar for a case-expression.
 , do   sp      <- pTokSP KMatch
        p       <- pPat c
        pTok KArrowDashLeft
        x1      <- pExp c
        pTok KElse
        x2      <- pExp c
        pTok KIn
        x3      <- pExp c
        return  $ XCase sp x1 [AAlt p x3, AAlt PDefault x2]


        -- weakeff [TYPE] in EXP
 , do   sp      <- pTokSP KWeakEff
        pTok KSquareBra
        t       <- pType c
        pTok KSquareKet
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastWeakenEffect t) x


        -- weakclo {EXP;+} in EXP
 , do   sp      <- pTokSP KWeakClo
        pTok KBraceBra
        xs      <- liftM (map fst . concat) 
                $  P.sepEndBy1 (pArgSPs c) (pTok KSemiColon)
        pTok KBraceKet
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastWeakenClosure xs) x


        -- purify WITNESS in EXP
 , do   sp      <- pTokSP KPurify
        w       <- pWitness c
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastPurify w) x


        -- forget WITNESS in EXP
 , do   sp      <- pTokSP KForget
        w       <- pWitness c
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastForget w) x


        -- box EXP
 , do   sp      <- pTokSP KBox
        x       <- pExp c
        return  $ XCast sp CastBox x


        -- run EXP
 , do   sp      <- pTokSP KRun
        x       <- pExp c
        return  $ XCast sp CastRun x


        -- APP
 , do   pExpApp c
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Ord n => Context -> Parser n (Exp SourcePos n)
pExpApp c
  = do  (x1, _)        <- pExpAtomSP c
        
        P.choice
         [ do   xs  <- liftM concat $ P.many1 (pArgSPs c)
                return  $ foldl (\x (x', sp) -> XApp sp x x') x1 xs

         ,      return x1]

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: Ord n => Context -> Parser n [(Exp SourcePos n, SourcePos)]
pArgSPs c
 = P.choice
        -- [TYPE]
 [ do   sp      <- pTokSP KSquareBra
        t       <- pType c
        pTok KSquareKet
        return  [(XType sp t, sp)]

        -- [: TYPE0 TYPE0 ... :]
 , do   sp      <- pTokSP KSquareColonBra
        ts      <- P.many1 (pTypeAtom c)
        pTok KSquareColonKet
        return  [(XType sp t, sp) | t <- ts]
        
        -- {WITNESS}
 , do   sp      <- pTokSP KBraceBra
        w       <- pWitness c
        pTok KBraceKet
        return  [(XWitness sp w, sp)]
                
        -- {: WITNESS0 WITNESS0 ... :}
 , do   sp      <- pTokSP KBraceColonBra
        ws      <- P.many1 (pWitnessAtom c)
        pTok KBraceColonKet
        return  [(XWitness sp w, sp) | w <- ws]
                
        -- EXP0
 , do   (x, sp)  <- pExpAtomSP c
        return  [(x, sp)]
 ]
 <?> "a type, witness or expression argument"


-- | Parse a variable, constructor or parenthesised expression.
pExpAtom   :: Ord n => Context -> Parser n (Exp SourcePos n)
pExpAtom c
 = do   (x, _) <- pExpAtomSP c
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP 
        :: Ord n 
        => Context 
        -> Parser n (Exp SourcePos n, SourcePos)

pExpAtomSP c
 = P.choice
        -- (EXP2)
 [ do   sp      <- pTokSP KRoundBra
        t       <- pExp c
        pTok KRoundKet
        return  (t, sp)
 
        -- The unit data constructor.       
 , do   sp              <- pTokSP KDaConUnit
        return  (XCon sp dcUnit, sp)

        -- Named algebraic constructors.
 , do   (con, sp)       <- pConSP
        return  (XCon sp (DaConBound con), sp)

        -- Literals.
        --   The attached type is set to Bottom for now, which needs
        --   to be filled in later by the Spread transform.
 , do   (lit, sp)       <- pLitSP
        return  (XCon sp (DaConPrim lit (T.tBot T.kData)), sp)

        -- Debruijn indices
 , do   (i, sp)         <- pIndexSP
        return  (XVar sp (UIx   i), sp)

        -- Variables
 , do   (var, sp)       <- pVarSP
        return  (XVar sp (UName var), sp)
 ]

 <?> "a variable, constructor, or parenthesised type"


-- Alternatives ---------------------------------------------------------------
-- Case alternatives.
pAlt    :: Ord n => Context -> Parser n (Alt SourcePos n)
pAlt c
 = do   p       <- pPat c
        pTok KArrowDash
        x       <- pExp c
        return  $ AAlt p x


-- Patterns.
pPat    :: Ord n 
        => Context -> Parser n (Pat n)
pPat c
 = P.choice
 [      -- Wildcard
   do   pTok KUnderscore
        return  $ PDefault

        -- LIT
 , do   --  The attached type is set to Bottom for now, which needs
        --  to be filled in later by the Spread transform.
        nLit    <- pLit
        return  $ PData (DaConPrim nLit (T.tBot T.kData)) []

        -- Unit
 , do   pTok KDaConUnit
        return  $ PData  dcUnit []

        -- CON BIND BIND ...
 , do   nCon    <- pCon 
        bs      <- liftM concat $ P.many (pBinds c)
        return  $ PData (DaConBound nCon) bs]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pBinds
        :: Ord n 
        => Context -> Parser n [Bind n]
pBinds c
 = P.choice
        -- Plain binder.
 [ do   bs      <- P.many1 pBinder
        return  [T.makeBindFromBinder b (T.tBot T.kData) | b <- bs]

        -- Binder with type, wrapped in parens.
 , do   pTok KRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KRoundKet
        return  [T.makeBindFromBinder b t | b <- bs]
 ]


-- Bindings -------------------------------------------------------------------
pLetsSP :: Ord n 
        => Context -> Parser n (Lets SourcePos n, SourcePos)
pLetsSP c
 = P.choice
    [ -- non-recursive let.
      do sp       <- pTokSP KLet
         (b1, x1) <- pLetBinding c
         return (LLet b1 x1, sp)

      -- recursive let.
    , do sp     <- pTokSP KLetRec
         P.choice
          -- Multiple bindings in braces
          [ do   pTok KBraceBra
                 lets    <- P.sepEndBy1 (pLetRecBinding c) (pTok KSemiColon)
                 pTok KBraceKet
                 return (LRec lets, sp)

          -- A single binding without braces.
          , do   ll      <- pLetRecBinding c
                 return (LRec [ll], sp)
          ]      

      -- Local region binding.
      --   letregions [BINDER] with { BINDER : TYPE ... } in EXP
      --   letregions [BINDER] in EXP
    , do sp     <- pTokSP KLetRegions
         brs    <- P.manyTill pBinder 
                $  P.try $ P.lookAhead $ P.choice [pTok KIn, pTok KWith]

         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs

         r      <- pLetWits c bs
         return (r, sp)
          
    , do sp     <- pTokSP KLetRegion
         br     <- pBinder
         let b  =  T.makeBindFromBinder br T.kRegion
         r      <- pLetWits c [b]
         return (r, sp)
         
    ]
    
    
pLetWits :: Ord n 
        => Context -> [Bind n] -> Parser n (Lets SourcePos n)

pLetWits c bs
 = P.choice 
    [ do   pTok KWith
           pTok KBraceBra
           wits    <- P.sepBy
                      (do  b    <- pBinder
                           pTok (KOp ":")
                           t    <- pTypeApp c
                           return  $ T.makeBindFromBinder b t)
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LLetRegions bs wits)
    
    , do   return (LLetRegions bs [])
    ]


-- | A binding for let expression.
pLetBinding 
        :: Ord n 
        => Context
        -> Parser n ( Bind n
                    , Exp SourcePos n)
pLetBinding c
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok (KOp ":")
                t       <- pType c
                pTok (KOp "=")
                xBody   <- pExp c

                return  $ (T.makeBindFromBinder b t, xBody) 


         , do   -- Non-function binding with no type signature.
                -- This form can't be used with letrec as we can't use it
                -- to build the full type sig for the let-bound variable.
                --  BINDER = EXP
                pTok (KOp "=")
                xBody   <- pExp c
                let t   = T.tBot T.kData
                return  $ (T.makeBindFromBinder b t, xBody)


         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many (pBindParamSpec c)
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                        pTok (KOp ":")
                        tBody   <- pType c
                        sp      <- pTokSP (KOp "=")
                        xBody   <- pExp c

                        let x   = expOfParams sp ps xBody
                        let t   = funTypeOfParams c ps tBody
                        return  (T.makeBindFromBinder b t, x)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --  BINDER PARAM1 PARAM2 .. PARAMN = EXP
                 , do   sp      <- pTokSP (KOp "=")
                        xBody   <- pExp c

                        let x   = expOfParams sp ps xBody
                        let t   = T.tBot T.kData
                        return  (T.makeBindFromBinder b t, x) ]
         ]

-- | Letrec bindings must have a full type signature, 
--   or use function syntax with a return type so that we can make one.
pLetRecBinding 
        :: Ord n 
        => Context
        -> Parser n (Bind n, Exp SourcePos n)

pLetRecBinding  c
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok (KOp ":")
                t       <- pType c
                pTok (KOp "=")
                xBody   <- pExp c

                return  $ (T.makeBindFromBinder b t, xBody) 


         , do   -- Binding using function syntax.
                --  BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                ps      <- liftM concat 
                        $  P.many (pBindParamSpec c)
        
                pTok (KOp ":")
                tBody   <- pType c
                let t   = funTypeOfParams c ps tBody

                sp      <- pTokSP (KOp "=")
                xBody   <- pExp c
                let x   = expOfParams sp ps xBody

                return  (T.makeBindFromBinder b t, x) ]


-- Statements -----------------------------------------------------------------
data Stmt n
        = StmtBind  SourcePos (Bind n) (Exp SourcePos n)
        | StmtMatch SourcePos (Pat n)  (Exp SourcePos n) (Exp SourcePos n)
        | StmtNone  SourcePos (Exp SourcePos n)


-- | Parse a single statement.
pStmt :: Ord n => Context -> Parser n (Stmt n)
pStmt c
 = P.choice
 [ -- BINDER = EXP ;
   -- We need the 'try' because a VARIABLE binders can also be parsed
   --   as a function name in a non-binding statement.
   --  
   P.try $ 
    do  br      <- pBinder
        sp      <- pTokSP (KOp "=")
        x1      <- pExp c
        let t   = T.tBot T.kData
        let b   = T.makeBindFromBinder br t
        return  $ StmtBind sp b x1

   -- PAT <- EXP else EXP;
   -- Sugar for a case-expression.
   -- We need the 'try' because the PAT can also be parsed
   --  as a function name in a non-binding statement.
 , P.try $
    do  p       <- pPat c
        sp      <- pTokSP KArrowDashLeft
        x1      <- pExp c
        pTok KElse
        x2      <- pExp c
        return  $ StmtMatch sp p x1 x2

        -- EXP
 , do   x               <- pExp c
        return  $ StmtNone (annotOfExp x) x
 ]


-- | Parse some statements.
pStmts :: Ord n => Context -> Parser n (Exp SourcePos n)
pStmts c
 = do   stmts   <- P.sepEndBy1 (pStmt c) (pTok KSemiColon)
        case makeStmts stmts of
         Nothing -> P.unexpected "do-block must end with a statement"
         Just x  -> return x


-- | Make an expression from some statements.
makeStmts :: [Stmt n] -> Maybe (Exp SourcePos n)
makeStmts ss
 = case ss of
        [StmtNone _ x]    
         -> Just x

        StmtNone sp x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet sp (LLet (BNone (T.tBot T.kData)) x1) x2

        StmtBind sp b x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet sp (LLet b x1) x2

        StmtMatch sp p x1 x2 : rest
         | Just x3      <- makeStmts rest
         -> Just $ XCase sp x1 
                 [ AAlt p x3
                 , AAlt PDefault x2]

        _ -> Nothing

