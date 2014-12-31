
-- | Core language parser.
module DDC.Source.Tetra.Parser.Exp
        ( context
        , pExp
        , pExpApp
        , pExpAtom,     pExpAtomSP
        , pLetsSP,      pLetBinding
        , pType
        , pTypeApp
        , pTypeAtom)
where
import DDC.Source.Tetra.Parser.Param
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp

import DDC.Core.Parser
        ( Parser
        , pBinder
        , pWitness
        , pWitnessAtom
        , pType
        , pTypeAtom
        , pTypeApp
        , pCon,         pConSP
        , pLit,         pLitSP
        ,               pStringSP
        , pIndexSP
        , pOpSP,        pOpVarSP
        , pVarSP
        , pTok
        , pTokSP)


import DDC.Core.Parser.Context
import DDC.Core.Lexer.Tokens
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import Control.Monad.Except


context :: Context Name
context = Context
        { contextTrackedEffects         = True
        , contextTrackedClosures        = True
        , contextFunctionalEffects      = False
        , contextFunctionalClosures     = False 
        , contextMakeStringName         = Just (\_ tx -> NameLitString tx) }



-- Exp --------------------------------------------------------------------------------------------
-- | Parse a Tetra Source language expression.
pExp    :: Context Name -> Parser Name (Exp SourcePos Name)
pExp c
 = P.choice
        -- Level-0 lambda abstractions
        -- \(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
 [ do   sp      <- pTokSP KBackSlash

        bs      <- liftM concat
                $  P.many1 
                $  do   pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

        pTok KDot
        xBody   <- pExp c
        return  $ foldr (XLam sp) xBody bs

        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
 , do   sp      <- pTokSP KBigLambda

        bs      <- liftM concat
                $  P.many1 
                $  do   pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

        pTok KDot
        xBody   <- pExp c
        return  $ foldr (XLAM sp) xBody bs

        -- let expression
 , do   (lts, sp) <- pLetsSP c
        pTok    KIn
        x2      <- pExp c
        return  $ XLet sp lts x2

        -- Sugar for a let-expression.
        --  do { Stmt;+ }
 , do   pTok    KDo
        pTok    KBraceBra
        xx      <- pStmts c
        pTok    KBraceKet
        return  $ xx

        -- case Exp of { Alt;+ }
 , do   sp      <- pTokSP KCase
        x       <- pExp c
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 (pAlt c) (pTok KSemiColon)
        pTok KBraceKet
        return  $ XCase sp x alts

        -- match { | EXP = EXP | EXP = EXP ... }
        --  Sugar for cascaded case expressions case-expression.
 , do   _       <- pTok KMatch
        pTok KBraceBra
        (_, x)  <- pMatchGuardsAsCase c
        pTok KBraceKet
        return x

 , do   -- if-then-else
        --  Sugar for a case-expression.
        sp      <- pTokSP KIf
        x1      <- pExp c
        pTok KThen
        x2      <- pExp c
        pTok KElse
        x3      <- pExp c
        return  $ XCase sp x1 
                        [ AAlt (PData (DaConPrim (NameLitBool True) tBool) []) [] x2
                        , AAlt PDefault [] x3 ]

        -- weakeff [Type] in Exp
 , do   sp      <- pTokSP KWeakEff
        pTok KSquareBra
        t       <- pType c
        pTok KSquareKet
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastWeakenEffect t) x

        -- purify Witness in Exp
 , do   sp      <- pTokSP KPurify
        w       <- pWitness c
        pTok KIn
        x       <- pExp c
        return  $ XCast sp (CastPurify w) x

        -- box Exp
 , do   sp      <- pTokSP KBox
        x       <- pExp c
        return  $ XCast sp CastBox x

        -- run Exp
 , do   sp      <- pTokSP KRun
        x       <- pExp c
        return  $ XCast sp CastRun x

        -- APP
 , do   pExpApp c
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Context Name -> Parser Name (Exp SourcePos Name)
pExpApp c
  = do  xps     <- liftM concat $ P.many1 (pArgSPs c)
        let (xs, sps)   = unzip xps
        let sp1 : _     = sps
                
        case xs of
         [x]    -> return x
         _      -> return $ XDefix sp1 xs

  <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: Context Name -> Parser Name [(Exp SourcePos Name, SourcePos)]
pArgSPs c
 = P.choice
        -- [Type]
 [ do   sp      <- pTokSP KSquareBra
        t       <- pType c
        pTok KSquareKet
        return  [(XType sp t, sp)]

        -- [: Type0 Type0 ... :]
 , do   sp      <- pTokSP KSquareColonBra
        ts      <- P.many1 (pTypeAtom c)
        pTok KSquareColonKet
        return  [(XType sp t, sp) | t <- ts]
        
        -- { Witness }
 , do   sp      <- pTokSP KBraceBra
        w       <- pWitness c
        pTok KBraceKet
        return  [(XWitness sp w, sp)]
                
        -- {: Witness0 Witness0 ... :}
 , do   sp      <- pTokSP KBraceColonBra
        ws      <- P.many1 (pWitnessAtom c)
        pTok KBraceColonKet
        return  [(XWitness sp w, sp) | w <- ws]
               
        -- Exp0
 , do   (x, sp)  <- pExpAtomSP c
        return  [(x, sp)]
 ]
 <?> "a type, witness or expression argument"


-- | Parse a variable, constructor or parenthesised expression.
pExpAtom   :: Context Name -> Parser Name (Exp SourcePos Name)
pExpAtom c
 = do   (x, _) <- pExpAtomSP c
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP :: Context Name -> Parser Name (Exp SourcePos Name, SourcePos)
pExpAtomSP c
 = P.choice
 [      -- ( Exp2 )
   do   sp      <- pTokSP KRoundBra
        t       <- pExp c
        pTok KRoundKet
        return  (t, sp)

        -- Infix operator used as a variable.
 , do   (str, sp) <- pOpVarSP
        return  (XInfixVar sp str, sp)

        -- Infix operator used nekkid.
 , do   (str, sp) <- pOpSP
        return  (XInfixOp sp str, sp)
  
        -- The unit data constructor.       
 , do   sp              <- pTokSP KDaConUnit
        return  (XCon sp dcUnit, sp)

        -- Named algebraic constructors.
 , do   (con, sp)       <- pConSP
        return  (XCon sp (DaConBound con), sp)

        -- Literals.
        --  We just fill-in the type with tBot for now, and leave it to
        --  the spreader to attach the real type.
        --  We also set the literal as being algebraic, which may not be
        --  true (as for Floats). The spreader also needs to fix this.
 , do   (lit, sp)       <- pLitSP
        return  (XCon sp (DaConPrim lit (T.tBot T.kData)), sp)

 , do   (tx, sp)        <- pStringSP
        let Just mkString = contextMakeStringName c 
        let lit           = mkString sp tx
        return  (XCon sp (DaConPrim lit (T.tBot T.kData)), sp)

        -- Debruijn indices
 , do   (i, sp)         <- pIndexSP
        return  (XVar sp (UIx   i), sp)

        -- Variables
 , do   (var, sp)       <- pVarSP
        return  (XVar sp (UName var), sp)
 ]

 <?> "a variable, constructor, or parenthesised type"


-- Alternatives -----------------------------------------------------------------------------------
-- Case alternatives.
pAlt    :: Context Name -> Parser Name (Alt SourcePos Name)
pAlt c
 = do   p       <- pPat c
        pTok KArrowDash
        x       <- pExp c
        return  $ AAlt p [] x


-- Patterns.
pPat    :: Context Name -> Parser Name (Pat Name)
pPat c
 = P.choice
 [      -- Wildcard
   do   pTok KUnderscore
        return  $ PDefault

        -- Lit
 , do   nLit    <- pLit
        return  $ PData (DaConPrim nLit (T.tBot T.kData)) []

        -- 'Unit'
 , do   pTok KDaConUnit
        return  $ PData  dcUnit []

        -- Con Bind Bind ...
 , do   nCon    <- pCon 
        bs      <- P.many (pBindPat c)
        return  $ PData (DaConBound nCon) bs]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pBindPat 
        :: Ord n 
        => Context n -> Parser n (Bind n)
pBindPat c
 = P.choice
        -- Plain binder.
 [ do   b       <- pBinder
        return  $ T.makeBindFromBinder b (T.tBot T.kData)

        -- Binder with type, wrapped in parens.
 , do   pTok KRoundBra
        b       <- pBinder
        pTok (KOp ":")
        t       <- pType c
        pTok KRoundKet
        return  $ T.makeBindFromBinder b t
 ]


-- Guards -----------------------------------------------------------------------------------------
-- | Parse some guards and auto-desugar them to a case-expression.
pBindGuardsAsCase
        :: Context Name
        -> Parser Name (SourcePos, Exp SourcePos Name)

pBindGuardsAsCase c
 = do   gg      <- P.many (pGuardedExp c)
        let ((sp, _, _) : _) = gg
        return  (sp, xCaseOfGuards gg)


pMatchGuardsAsCase
        :: Context Name
        -> Parser Name (SourcePos, Exp SourcePos Name)

pMatchGuardsAsCase c
 = do   gg      <- P.sepEndBy1 (pGuardedExp c) (pTok KSemiColon)
        let ((sp, _, _) : _) = gg
        return  (sp, xCaseOfGuards gg)


-- | An guarded expression,
--   like | EXP1 = EXP2.
pGuardedExp 
        :: Context Name 
        -> Parser  Name (SourcePos, Maybe (Exp SourcePos Name), Exp SourcePos Name)

pGuardedExp c
 = do   sp      <- pTokSP KBar

        P.choice
         [ do   pTok KOtherwise
                pTok KEquals
                x       <- pExp c
                return  (sp, Nothing, x)

         , do   g       <- pExp c
                pTok KEquals
                x       <- pExp c
                return  (sp, Just g, x) ]


-- | Deguar some guards to a case-expression.
xCaseOfGuards
        :: [(SourcePos, Maybe (Exp SourcePos Name), Exp SourcePos Name)]
        -> Exp SourcePos Name
xCaseOfGuards gs 
 = go gs
 where
        go ((_, Nothing, x1) : [])
         = x1

        go ((spg, Just g1, x1) : (_, Nothing, x2) : [])
         = XCase spg g1
                [ AAlt (PData (DaConPrim (NameLitBool True) tBool) []) [] x1
                , AAlt PDefault [] x2 ]

        go ((spg, Just g, x) : gss)
         = XCase spg g 
                [ AAlt (PData (DaConPrim (NameLitBool True) tBool) []) [] x
                , AAlt PDefault [] (go gss) ]

        go _ = error "ddc-source-tetra: bad alts"    
                        -- TODO: panicify
        

-- Bindings ---------------------------------------------------------------------------------------
pLetsSP :: Context Name -> Parser Name (Lets SourcePos Name, SourcePos)
pLetsSP c
 = P.choice
    [ -- non-recursive let
      do sp       <- pTokSP KLet
         (b1, x1) <- pLetBinding c
         return (LLet b1 x1, sp)

      -- recursive let
    , do sp       <- pTokSP KLetRec
         pTok KBraceBra
         lets     <- P.sepEndBy1 (pLetBinding c) (pTok KSemiColon)
         pTok KBraceKet
         return (LRec lets, sp)

      -- Private region binding.
      --   private Binder+ (with { Binder : Type ... })? in Exp
    , do sp     <- pTokSP KPrivate
         
        -- new private region names.
         brs    <- P.manyTill pBinder 
                $  P.try $ P.lookAhead $ P.choice [pTok KIn, pTok KWith]

         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs
         
         -- Witness types.
         r      <- pLetWits c bs Nothing
         return (r, sp)

      -- Extend an existing region.
      --   extend Binder+ using Type (with { Binder : Type ...})? in Exp
    , do sp     <- pTokSP KExtend

         -- parent region
         t      <- pType c
         pTok KUsing

         -- new private region names.
         brs    <- P.manyTill pBinder 
                $  P.try $ P.lookAhead 
                         $ P.choice [pTok KUsing, pTok KWith, pTok KIn]

         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs
         
         -- witness types
         r      <- pLetWits c bs (Just t)
         return (r, sp)
    ]
    
    
pLetWits 
        :: Ord n 
        => Context n
        -> [Bind n] -> Maybe (Type n)
        -> Parser n (Lets SourcePos n)

pLetWits c bs mParent
 = P.choice 
    [ do   pTok KWith
           pTok KBraceBra
           wits    <- P.sepBy (P.choice
                      [ -- Named witness binder.
                        do b    <- pBinder
                           pTok (KOp ":")
                           t    <- pTypeApp c
                           return  $ T.makeBindFromBinder b t

                        -- Ambient witness binding, used for capabilities.
                      , do t    <- pTypeApp c
                           return  $ BNone t
                      ])
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LPrivate bs mParent wits)
    
    , do   return (LPrivate bs mParent [])
    ]


-- | A binding for let expression.
pLetBinding 
        :: Context Name
        -> Parser Name ( Bind Name
                       , Exp SourcePos Name )
pLetBinding c
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  Binder : Type = Exp
                pTok (KOp ":")
                t       <- pType c

                xBody   <- P.choice 
                                [ pTok KEquals >> pExp c
                                , liftM snd $ pBindGuardsAsCase c ]

                return  $ (T.makeBindFromBinder b t, xBody) 

         , do   -- Non-function binding with no type signature.
                -- This form can't be used with letrec as we can't use it
                -- to build the full type sig for the let-bound variable.
                --   Binder = Exp
                pTok KEquals
                xBody   <- pExp c
                let t   = T.tBot T.kData
                return  $ (T.makeBindFromBinder b t, xBody)


         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many (pBindParamSpec c)
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   Binder Param1 Param2 .. ParamN : Type = Exp
                        pTok (KOp ":")
                        tBody   <- pType c

                        (sp, xBody)
                                <- P.choice
                                [ do    sp <- pTokSP KEquals 
                                        x  <- pExp c
                                        return (sp, x)
                                , do    pBindGuardsAsCase c ]

                        let x   = expOfParams sp ps xBody
                        let t   = funTypeOfParams c ps tBody
                        return  (T.makeBindFromBinder b t, x)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --   Binder Param1 Param2 .. ParamN = Exp
                 , do   (sp, xBody) 
                                <- P.choice
                                [ do    sp <- pTokSP KEquals
                                        x  <- pExp c
                                        return (sp, x)
                                , do    pBindGuardsAsCase c ]

                        let x   = expOfParams sp ps xBody
                        let t   = T.tBot T.kData
                        return  (T.makeBindFromBinder b t, x) ]
         ]


-- Statements -------------------------------------------------------------------------------------
data Stmt n
        = StmtBind  SourcePos (Bind n) (Exp SourcePos n)
        | StmtMatch SourcePos (Pat n)  (Exp SourcePos n) (Exp SourcePos n)
        | StmtNone  SourcePos (Exp SourcePos n)


-- | Parse a single statement.
pStmt :: Context Name -> Parser Name (Stmt Name)
pStmt c
 = P.choice
 [ -- Binder = Exp ;
   -- We need the 'try' because a VARIABLE binders can also be parsed
   --   as a function name in a non-binding statement.
   --  
   P.try $ 
    do  br      <- pBinder
        sp      <- pTokSP KEquals
        x1      <- pExp c
        let t   = T.tBot T.kData
        let b   = T.makeBindFromBinder br t
        return  $ StmtBind sp b x1

   -- Pat <- Exp else Exp ;
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

        -- Exp
 , do   x               <- pExp c

        -- This should always succeed because pExp doesn't
        -- parse plain types or witnesses
        let Just sp     = takeAnnotOfExp x
        
        return  $ StmtNone sp x
 ]


-- | Parse some statements.
pStmts :: Context Name -> Parser Name (Exp SourcePos Name)
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
                 [ AAlt p        [] x3
                 , AAlt PDefault [] x2]

        _ -> Nothing

