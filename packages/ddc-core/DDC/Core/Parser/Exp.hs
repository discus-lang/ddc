
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
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Compounds
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import Control.Monad.Error


-- Expressions ----------------------------------------------------------------
-- | Parse a core language expression.
pExp    :: Ord n => Parser n (Exp SourcePos n)
pExp 
 = P.choice
        -- Level-0 lambda abstractions
        -- \(x1 x2 ... : TYPE) (y1 y2 ... : TYPE) ... . EXP
 [ do   sp      <- pTokSP KBackSlash

        bs      <- liftM concat
                $  P.many1 
                $  do   pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok KColon
                        t       <- pType
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

        pTok KDot
        xBody   <- pExp
        return  $ foldr (XLam sp) xBody bs

        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : TYPE) (y1 y2 ... : TYPE) ... . EXP
 , do   sp      <- pTokSP KBigLambda

        bs      <- liftM concat
                $  P.many1 
                $  do   pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok KColon
                        t       <- pType
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

        pTok KDot
        xBody   <- pExp
        return  $ foldr (XLAM sp) xBody bs


        -- let expression
 , do   (lts, sp) <- pLetsSP
        pTok    KIn
        x2      <- pExp
        return  $ XLet sp lts x2


        -- do { STMTS }
        --   Sugar for a let-expression.
 , do   pTok    KDo
        pTok    KBraceBra
        xx      <- pStmts
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
        x       <- pExp
        return  $ XLet sp (LWithRegion u) x


        -- case EXP of { ALTS }
 , do   sp      <- pTokSP KCase
        x       <- pExp
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 pAlt (pTok KSemiColon)
        pTok KBraceKet
        return  $ XCase sp x alts


        -- match PAT <- EXP else EXP in EXP
        --  Sugar for a case-expression.
 , do   sp      <- pTokSP KMatch
        p       <- pPat
        pTok KArrowDashLeft
        x1      <- pExp 
        pTok KElse
        x2      <- pExp 
        pTok KIn
        x3      <- pExp
        return  $ XCase sp x1 [AAlt p x3, AAlt PDefault x2]


        -- weakeff [TYPE] in EXP
 , do   sp      <- pTokSP KWeakEff
        pTok KSquareBra
        t       <- pType
        pTok KSquareKet
        pTok KIn
        x       <- pExp
        return  $ XCast sp (CastWeakenEffect t) x


        -- weakclo {EXP;+} in EXP
 , do   sp      <- pTokSP KWeakClo
        pTok KBraceBra
        xs      <- liftM (map fst . concat) 
                $  P.sepEndBy1 pArgSPs (pTok KSemiColon)
        pTok KBraceKet
        pTok KIn
        x       <- pExp
        return  $ XCast sp (CastWeakenClosure xs) x


        -- purify <WITNESS> in EXP
 , do   sp      <- pTokSP KPurify
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        pTok KIn
        x       <- pExp
        return  $ XCast sp (CastPurify w) x


        -- forget <WITNESS> in EXP
 , do   sp      <- pTokSP KForget
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        pTok KIn
        x       <- pExp
        return  $ XCast sp (CastForget w) x

        -- suspend EXP
 , do   sp      <- pTokSP KSuspend
        x       <- pExp
        return  $ XCast sp CastSuspend x

        -- run EXP
 , do   sp      <- pTokSP KRun
        x       <- pExp
        return  $ XCast sp CastRun x

        -- APP
 , do   pExpApp
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Ord n => Parser n (Exp SourcePos n)
pExpApp 
  = do  (x1, _)        <- pExpAtomSP
        
        P.choice
         [ do   xs  <- liftM concat $ P.many1 pArgSPs
                return  $ foldl (\x (x', sp) -> XApp sp x x') x1 xs

         ,      return x1]

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: Ord n => Parser n [(Exp SourcePos n, SourcePos)]
pArgSPs 
 = P.choice
        -- [TYPE]
 [ do   sp      <- pTokSP KSquareBra
        t       <- pType 
        pTok KSquareKet
        return  [(XType t, sp)]

        -- [: TYPE0 TYPE0 ... :]
 , do   sp      <- pTokSP KSquareColonBra
        ts      <- P.many1 pTypeAtom
        pTok KSquareColonKet
        return  [(XType t, sp) | t <- ts]
        
        -- <WITNESS>
 , do   sp      <- pTokSP KAngleBra
        w       <- pWitness
        pTok KAngleKet
        return  [(XWitness w, sp)]
                
        -- <: WITNESS0 WITNESS0 ... :>
 , do   sp      <- pTokSP KAngleColonBra
        ws      <- P.many1 pWitnessAtom
        pTok KAngleColonKet
        return  [(XWitness w, sp) | w <- ws]
                
        -- EXP0
 , do   (x, sp)  <- pExpAtomSP
        return  [(x, sp)]
 ]
 <?> "a type, witness or expression argument"


-- | Parse a variable, constructor or parenthesised expression.
pExpAtom   :: Ord n => Parser n (Exp SourcePos n)
pExpAtom 
 = do   (x, _) <- pExpAtomSP
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP :: Ord n => Parser n (Exp SourcePos n, SourcePos)
pExpAtomSP 
 = P.choice
        -- (EXP2)
 [ do   sp      <- pTokSP KRoundBra
        t       <- pExp
        pTok KRoundKet
        return  (t, sp)
 
        -- The unit data constructor.       
 , do   sp              <- pTokSP KDaConUnit
        return  (XCon sp dcUnit, sp)

        -- Named algebraic constructors.
        --  We just fill-in the type with tBot for now, and leave it to 
        --  the spreader to attach the real type.
 , do   (con, sp)       <- pConSP
        return  (XCon sp (mkDaConAlg con (T.tBot T.kData)), sp)

        -- Literals.
        --  We just fill-in the type with tBot for now, and leave it to
        --  the spreader to attach the real type.
        --  We also set the literal as being algebraic, which may not be
        --  true (as for Floats). The spreader also needs to fix this.
 , do   (lit, sp)       <- pLitSP
        return  (XCon sp (mkDaConAlg lit (T.tBot T.kData)), sp)

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
pAlt    :: Ord n => Parser n (Alt SourcePos n)
pAlt
 = do   p       <- pPat
        pTok KArrowDash
        x       <- pExp
        return  $ AAlt p x


-- Patterns.
pPat    :: Ord n => Parser n (Pat n)
pPat
 = P.choice
 [      -- Wildcard
   do   pTok KUnderscore
        return  $ PDefault

        -- LIT
 , do   nLit    <- pLit
        return  $ PData (mkDaConAlg nLit (T.tBot T.kData)) []

        -- Unit
 , do   pTok KDaConUnit
        return  $ PData  dcUnit []

        -- CON BIND BIND ...
 , do   nCon    <- pCon 
        bs      <- P.many pBindPat
        return  $ PData (mkDaConAlg nCon (T.tBot T.kData)) bs]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pBindPat :: Ord n => Parser n (Bind n)
pBindPat 
 = P.choice
        -- Plain binder.
 [ do   b       <- pBinder
        return  $ T.makeBindFromBinder b (T.tBot T.kData)

        -- Binder with type, wrapped in parens.
 , do   pTok KRoundBra
        b       <- pBinder
        pTok KColon
        t       <- pType
        pTok KRoundKet
        return  $ T.makeBindFromBinder b t
 ]


-- Bindings -------------------------------------------------------------------
pLetsSP :: Ord n => Parser n (Lets SourcePos n, SourcePos)
pLetsSP
 = P.choice
    [ -- non-recursive let.
      do sp       <- pTokSP KLet
         (b1, x1) <- pLetBinding
         return (LLet b1 x1, sp)

      -- recursive let.
    , do sp     <- pTokSP KLetRec
         P.choice
          -- Multiple bindings in braces
          [ do   pTok KBraceBra
                 lets    <- P.sepEndBy1 pLetRecBinding (pTok KSemiColon)
                 pTok KBraceKet
                 return (LRec lets, sp)

          -- A single binding without braces.
          , do   ll      <- pLetRecBinding
                 return (LRec [ll], sp)
          ]      

      -- Local region binding.
      --   letregions [BINDER] with { BINDER : TYPE ... } in EXP
      --   letregions [BINDER] in EXP
    , do sp     <- pTokSP KLetRegions
         brs    <- P.manyTill pBinder (P.try $ P.lookAhead $ P.choice [pTok KIn, pTok KWith])
         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs
         r      <- pLetWits bs
         return (r, sp)
          
    , do sp     <- pTokSP KLetRegion
         br    <- pBinder
         let b =  T.makeBindFromBinder br T.kRegion
         r      <- pLetWits [b]
         return (r, sp)
         
    ]
    
    
pLetWits :: Ord n => [Bind n] -> Parser n (Lets SourcePos n)
pLetWits bs
 = P.choice 
    [ do   pTok KWith
           pTok KBraceBra
           wits    <- P.sepBy
                      (do  b    <- pBinder
                           pTok KColon
                           t    <- pTypeApp
                           return  $ T.makeBindFromBinder b t)
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LLetRegions bs wits)
    
    , do   return (LLetRegions bs [])
    ]


-- | A binding for let expression.
pLetBinding 
        :: Ord n 
        => Parser n ( Bind n
                    , Exp SourcePos n)
pLetBinding 
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok KColon
                t       <- pType
                pTok KEquals
                xBody   <- pExp

                return  $ (T.makeBindFromBinder b t, xBody) 


         , do   -- Non-function binding with no type signature.
                -- This form can't be used with letrec as we can't use it
                -- to build the full type sig for the let-bound variable.
                --  BINDER = EXP
                pTok KEquals
                xBody   <- pExp
                let t   = T.tBot T.kData
                return  $ (T.makeBindFromBinder b t, xBody)


         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many pBindParamSpec 
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                        pTok KColon
                        tBody   <- pType
                        sp      <- pTokSP KEquals
                        xBody   <- pExp

                        let x   = expOfParams sp ps xBody
                        let t   = funTypeOfParams ps tBody
                        return  (T.makeBindFromBinder b t, x)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --  BINDER PARAM1 PARAM2 .. PARAMN = EXP
                 , do   sp      <- pTokSP KEquals
                        xBody   <- pExp

                        let x   = expOfParams sp ps xBody
                        let t   = T.tBot T.kData
                        return  (T.makeBindFromBinder b t, x) ]
         ]

-- | Letrec bindings must have a full type signature, 
--   or use function syntax with a return type so that we can make one.
pLetRecBinding :: Ord n => Parser n (Bind n, Exp SourcePos n)
pLetRecBinding 
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok KColon
                t       <- pType
                pTok KEquals
                xBody   <- pExp

                return  $ (T.makeBindFromBinder b t, xBody) 


         , do   -- Binding using function syntax.
                --  BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                ps      <- liftM concat 
                        $  P.many pBindParamSpec 
        
                pTok KColon
                tBody   <- pType
                let t   = funTypeOfParams ps tBody

                sp      <- pTokSP KEquals
                xBody   <- pExp
                let x   = expOfParams sp ps xBody

                return  (T.makeBindFromBinder b t, x) ]


-- Statements -----------------------------------------------------------------
data Stmt n
        = StmtBind  SourcePos (Bind n) (Exp SourcePos n)
        | StmtMatch SourcePos (Pat n)  (Exp SourcePos n) (Exp SourcePos n)
        | StmtNone  SourcePos (Exp SourcePos n)


-- | Parse a single statement.
pStmt :: Ord n => Parser n (Stmt n)
pStmt 
 = P.choice
 [ -- BINDER = EXP ;
   -- We need the 'try' because a VARIABLE binders can also be parsed
   --   as a function name in a non-binding statement.
   --  
   P.try $ 
    do  br      <- pBinder
        sp      <- pTokSP    KEquals
        x1      <- pExp
        let t   = T.tBot T.kData
        let b   = T.makeBindFromBinder br t
        return  $ StmtBind sp b x1

   -- PAT <- EXP else EXP;
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

        -- EXP
 , do   x               <- pExp

        -- This should always succeed because pExp doesn't
        -- parse plain types or witnesses
        let Just sp     = takeAnnotOfExp x
        
        return  $ StmtNone sp x
 ]


-- | Parse some statements.
pStmts :: Ord n => Parser n (Exp SourcePos n)
pStmts
 = do   stmts   <- P.sepEndBy1 pStmt (pTok KSemiColon)
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

