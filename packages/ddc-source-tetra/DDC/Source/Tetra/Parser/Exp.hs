
-- | Parser for Source Tetra expressions.
module DDC.Source.Tetra.Parser.Exp
        ( context
        , pExp
        , pExpApp
        , pExpAtom,     pExpAtomSP
        , pLetsSP,      pClauseSP
        , pType
        , pTypeApp
        , pTypeAtom)
where
import DDC.Source.Tetra.Transform.Guards
import DDC.Source.Tetra.Parser.Witness
import DDC.Source.Tetra.Parser.Param
import DDC.Source.Tetra.Parser.Atom
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp.Annot
import DDC.Core.Lexer.Tokens
import Control.Monad.Except
import DDC.Base.Parser                  ((<?>), SourcePos(..))
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Exp.Simple    as T
import qualified Data.Text              as Text

import DDC.Core.Parser
        ( Parser
        , Context (..)
        , pBinder
        , pType
        , pTypeAtom
        , pTypeApp
        , pCon,         pConSP
        , pLit,         pLitSP
        ,               pStringSP
        , pIndexSP
        , pOpSP,        pOpVarSP
        , pTok
        , pTokSP)


type SP = SourcePos

-- | Starting context for the parser.
--   Holds flags about what language features we should accept.
context :: Context Name
context = Context
        { contextTrackedEffects         = True
        , contextTrackedClosures        = True
        , contextFunctionalEffects      = False
        , contextFunctionalClosures     = False 
        , contextMakeStringName         = Just (\_ tx -> NameLitTextLit tx) }


-- Exp --------------------------------------------------------------------------------------------
-- | Parse a Tetra Source language expression.
pExp    :: Context Name -> Parser Name (Exp SP)
pExp c
 = P.choice

        -- Level-0 lambda abstractions
        --  \(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
        --  \x1 x2 : Type. Exp
 [ do   sp      <- P.choice [ pTokSP KLambda, pTokSP KBackSlash ]

        bs      <- P.choice
                [ fmap concat $ P.many1 
                   $ do pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

                , do    bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        return (map (\b -> T.makeBindFromBinder b t) bs') 
                ]

        pTok KDot
        xBody   <- pExp c
        return  $ foldr (XLam sp) xBody bs

        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
 , do   sp      <- P.choice [ pTokSP KBigLambda, pTokSP KBigLambdaSlash ]

        bs      <- P.choice
                [ fmap concat $ P.many1
                   $ do pTok KRoundBra
                        bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        pTok KRoundKet
                        return (map (\b -> T.makeBindFromBinder b t) bs')

                , do    bs'     <- P.many1 pBinder
                        pTok (KOp ":")
                        t       <- pType c
                        return (map (\b -> T.makeBindFromBinder b t) bs')
                ]

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
 , do   sp      <- pTokSP KMatch
        pTok KBraceBra
        x       <- pMatchGuardsAsCase sp c
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
                        [ AAlt pTrue    [GExp x2]
                        , AAlt PDefault [GExp x3]]

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
pExpApp :: Context Name -> Parser Name (Exp SP)
pExpApp c
  = do  xps     <- liftM concat $ P.many1 (pArgSPs c)
        let (xs, sps)   = unzip xps
        let sp1 : _     = sps
                
        case xs of
         [x]    -> return x
         _      -> return $ XDefix sp1 xs

  <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: Context Name -> Parser Name [(Exp SP, SP)]
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
pExpAtom   :: Context Name -> Parser Name (Exp SP)
pExpAtom c
 = do   (x, _) <- pExpAtomSP c
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP :: Context Name -> Parser Name (Exp SP, SP)
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

        -- Primitive names.
 , do   (nPrim, sp)     <- pPrimValSP
        return  (XPrim sp nPrim, sp)

        -- Named variables.
 , do   (sVar,  sp)     <- pVarStringSP
        return  (XVar  sp (T.UName (NameVar sVar)), sp)

        -- Debruijn indices
 , do   (i, sp)         <- pIndexSP
        return  (XVar  sp (T.UIx   i), sp)

 ]

 <?> "a variable, constructor, or parenthesised type"


-- Alternatives -----------------------------------------------------------------------------------
-- Case alternatives.
pAlt    :: Context Name -> Parser Name (Alt SP)
pAlt c
 = do   p       <- pPat c
        P.choice
         [ do   -- Desugar case guards while we're here.
                spgxs     <- P.many1 (pGuardedExpSP c (pTokSP KArrowDash))
                let gxs  = map snd spgxs
                return  $ AAlt p gxs 
                
         , do   pTok KArrowDash
                x       <- pExp c
                return  $ AAlt p [GExp x] ]


-- Patterns.
pPat    :: Context Name -> Parser Name (Pat SP)
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
pBindPat :: Context Name -> Parser Name Bind
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
pBindGuardsAsCaseSP
        :: Context Name
        -> Parser Name (SP, Exp SP)

pBindGuardsAsCaseSP c
 = do   
        (sp, g) : spgs  
                <- P.many1 (pGuardedExpSP c (pTokSP KEquals))

        -- Desugar guards.
        -- If none match then raise a runtime error.
        let xx' = desugarGuards sp (g : map snd spgs)  
                $ xErrorDefault sp 
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)

        return  (sp, xx')


pMatchGuardsAsCase
        :: SP -> Context Name
        -> Parser Name (Exp SP)

pMatchGuardsAsCase sp c
 = do   gg      <- liftM (map snd)
                $  P.sepEndBy1  (pGuardedExpSP c (pTokSP KEquals)) 
                                (pTok KSemiColon)

        -- Desugar guards.
        -- If none match then raise a runtime error.
        let xx' = desugarGuards sp gg
                $ xErrorDefault sp 
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)

        return  xx'


-- | An guarded expression,
--   like | EXP1 = EXP2.
pGuardedExpSP 
        :: Context Name         -- ^ Parser context.
        -> Parser  Name SP      -- ^ Parser for char between and of guards and exp.
                                --   usually -> or =
        -> Parser  Name (SP, GuardedExp SP)

pGuardedExpSP c pTermSP
 = pGuardExp (pTokSP KBar)

 where  pGuardExp pSepSP
         = P.choice
         [ do   sp      <- pSepSP
                g       <- pGuard
                gx      <- liftM snd $ pGuardExp (pTokSP KComma)
                return  (sp, GGuard g gx)

         , do   sp      <- pTermSP
                x       <- pExp c
                return  (sp, GExp x) ]

        pGuard
         = P.choice 
         [ P.try $
           do   p       <- pPat c
                pTok KArrowDashLeft
                x       <- pExp c
                return $ GPat p x

         , do   g       <- pExp c
                return $ GPred g


         , do   pTok KOtherwise
                return GDefault ]


-- Bindings ---------------------------------------------------------------------------------------
pLetsSP :: Context Name -> Parser Name (Lets SP, SP)
pLetsSP c
 = P.choice
    [ -- non-recursive let
      do sp       <- pTokSP KLet
         l        <- liftM fst $ pClauseSP c
         return (LGroup [l], sp)

      -- recursive let
    , do sp       <- pTokSP KLetRec
         pTok KBraceBra
         ls       <- liftM (map fst)
                  $  P.sepEndBy1 (pClauseSP c) (pTok KSemiColon)
         pTok KBraceKet
         return (LGroup ls, sp)

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
    
    
pLetWits :: Context Name
         -> [Bind] -> Maybe (T.Type Name)
         -> Parser Name (Lets SP)

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
                           return  $ T.BNone t
                      ])
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LPrivate bs mParent wits)
    
    , do   return (LPrivate bs mParent [])
    ]


-- | A binding for let expression.
pClauseSP :: Context Name
          -> Parser Name (Clause SP, SP)

pClauseSP c
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                sp         <- pTokSP (KOp ":")
                t          <- pType c
                (_, xBody) <- pBindGuardsAsCaseSP c
                return  ( SLet sp (T.makeBindFromBinder b t) [] [GExp xBody]
                        , sp)

         , do   -- Non-function binding with no type signature.
                sp      <- pTokSP KEquals
                xBody   <- pExp c
                let t   = T.tBot T.kData
                return  ( SLet sp (T.makeBindFromBinder b t) [] [GExp xBody]
                        , sp)

         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many (pBindParamSpec c)
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   Binder Param1 Param2 .. ParamN : Type = Exp
                        pTok (KOp ":")
                        tBody       <- pType c
                        (sp, xBody) <- pBindGuardsAsCaseSP c

                        let x   = expOfParams sp ps xBody
                        let t   = funTypeOfParams c ps tBody
                        return  ( SLet sp (T.makeBindFromBinder b t) [] [GExp x]
                                , sp)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --   Binder Param1 Param2 .. ParamN = Exp
                 , do   (sp, xBody) <- pBindGuardsAsCaseSP c

                        let x   = expOfParams sp ps xBody
                        let t   = T.tBot T.kData
                        return  ( SLet sp (T.makeBindFromBinder b t) [] [GExp x]
                                , sp)
                 ]
         ]


-- Statements -------------------------------------------------------------------------------------
data Stmt n
        = StmtBind  SP Bind (Exp SP)
        | StmtMatch SP (Pat SP) (Exp SP) (Exp SP)
        | StmtNone  SP (Exp SP)


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
pStmts :: Context Name -> Parser Name (Exp SP)
pStmts c
 = do   stmts   <- P.sepEndBy1 (pStmt c) (pTok KSemiColon)
        case makeStmts stmts of
         Nothing -> P.unexpected "do-block must end with a statement"
         Just x  -> return x


-- | Make an expression from some statements.
makeStmts :: [Stmt Name] -> Maybe (Exp SP)
makeStmts ss
 = case ss of
        [StmtNone _ x]    
         -> Just x

        StmtNone sp x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet sp (LLet (T.BNone (T.tBot T.kData)) x1) x2

        StmtBind sp b x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet sp (LLet b x1) x2

        StmtMatch sp p x1 x2 : rest
         | Just x3      <- makeStmts rest
         -> Just $ XCase sp x1 
                 [ AAlt p        [GExp x3]
                 , AAlt PDefault [GExp x2] ]

        _ -> Nothing

