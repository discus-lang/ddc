
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
import DDC.Core.Exp.Annot
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Param
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Data.Pretty
import DDC.Control.Parser               ((<?>), SourcePos)
import qualified DDC.Control.Parser     as P
import qualified DDC.Type.Exp.Simple    as T
import Control.Monad.Except


-- Exp --------------------------------------------------------------------------------------------
-- | Parse a core language expression.
pExp    :: (Ord n, Pretty n)
        => Context n -> Parser n (Exp SourcePos n)
pExp c
 = P.choice
        -- Level-0 lambda abstractions
        -- (λBIND.. . EXP) or (\BIND.. . EXP)
 [ do   sp      <- P.choice [ pSym SLambda,    pSym SBackSlash]
        params  <- liftM concat $ P.many1 (pParams c)
        pSym    SDot
        xBody   <- pExp c
        return  $ foldr (XAbs sp) xBody params

        -- Level-1 lambda abstractions.
        -- (ΛBINDS.. . EXP) or (/\BIND.. . EXP)
 , do   sp      <- P.choice [ pSym SBigLambda, pSym SBigLambdaSlash]
        bs      <- liftM concat $ P.many1 (pTypeBinds c)
        pSym    SDot
        xBody   <- pExp c
        return  $ foldr (XLAM sp) xBody bs

        -- let expression
 , do   (lts, sp) <- pLetsSP c
        pKey    EIn
        x2      <- pExp c
        return  $ foldr (XLet sp) x2 lts

        -- do { STMTS }
        --   Sugar for a let-expression.
 , do   pKey    EDo
        pSym    SBraceBra
        xx      <- pStmts c
        pSym    SBraceKet
        return  $ xx

        -- case EXP of { ALTS }
 , do   sp      <- pKey ECase
        x       <- pExp c
        pKey    EOf
        pSym    SBraceBra
        alts    <- P.sepEndBy1 (pAlt c) (pSym SSemiColon)
        pSym    SBraceKet
        return  $ XCase sp x alts

        -- letcase PAT = EXP in EXP
 , do   --  Sugar for a single-alternative case expression.
        sp      <- pKey ELetCase
        P.choice
         [ do   pSym SBraceBra
                p       <- pPat c
                pSym    SEquals
                x1      <- pExp c
                pSym SBraceKet
                pKey    EIn
                x2      <- pExp c
                return  $ XCase sp x1 [AAlt p x2]
         ]

        -- weakeff [TYPE] in EXP
 , do   sp      <- pKey EWeakEff
        pSym    SSquareBra
        t       <- pType c
        pSym    SSquareKet
        pKey    EIn
        x       <- pExp c
        return  $ XCast sp (CastWeakenEffect t) x

        -- purify WITNESS in EXP
 , do   sp      <- pKey EPurify
        w       <- pWitness c
        pTok (KKeyword EIn)
        x       <- pExp c
        return  $ XCast sp (CastPurify w) x

        -- box EXP
 , do   sp      <- pKey EBox
        x       <- pExp c
        return  $ XCast sp CastBox x

        -- run EXP
 , do   sp      <- pKey ERun
        x       <- pExp c
        return  $ XCast sp CastRun x

        -- APP
 , do   pExpApp c
 ]

 <?> "an expression"


-- | Parse a function application.
pExpApp :: (Ord n, Pretty n)
        => Context n -> Parser n (Exp SourcePos n)
pExpApp c
  = do  (x1, _)        <- pExpAtomSP c

        P.choice
         [ do   xs  <- liftM concat $ P.many1 (pArgSPs c)
                return  $ foldl (\x (x', sp) -> XApp sp x x') x1 xs

         ,      return x1]

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgSPs :: (Ord n, Pretty n)
        => Context n -> Parser n [(Arg SourcePos n, SourcePos)]
pArgSPs c
 = P.choice
        -- [TYPE]
 [ do   sp      <- pSym SSquareBra
        t       <- pType c
        pSym SSquareKet
        return  [(RType t, sp)]

        -- [: TYPE0 TYPE0 ... :]
 , do   sp      <- pSym SSquareColonBra
        ts      <- P.many1 (pTypeAtom c)
        pSym SSquareColonKet
        return  [(RType t, sp) | t <- ts]

        -- <WITNESS>
 , do   sp      <- pTokSP (KOp "<")
        w       <- pWitness c
        pTok (KOp ">")
        return  [(RWitness w, sp)]

        -- {EXP}
 , do   sp      <- pSym SBraceBra
        x       <- pExp c
        pSym SBraceKet
        return  [(RImplicit (RTerm x), sp)]

        -- EXP0
 , do   (x, sp)  <- pExpAtomSP c
        return  [(RTerm x, sp)]
 ]
 <?> "a type, witness or expression argument"


-- | Parse a variable, constructor or parenthesised expression.
pExpAtom :: (Ord n, Pretty n)
         => Context n -> Parser n (Exp SourcePos n)
pExpAtom c
 = do   (x, _) <- pExpAtomSP c
        return x


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP
        :: (Ord n, Pretty n)
        => Context n
        -> Parser n (Exp SourcePos n, SourcePos)

pExpAtomSP c
 = P.choice

 [      -- Record data constructor.
   P.try $ do
        sp      <- pSym SRoundBra
        ns      <- P.sepBy pVarName (pSym SComma)
        pSym SRoundKet
        pSym SHash
        return  (XCon sp (DaConRecord ns), sp)

        -- The syntax for the nullary record type constructor '()#' overlaps
        -- with that of the unit data construtor '()', so try the former first.
 , P.try $ do
        sp      <- pTokSP (KBuiltin BDaConUnit)
        pSym SHash
        return  (XCon sp (DaConRecord []), sp)

      -- (EXP2)
 , do   sp      <- pSym SRoundBra
        t       <- pExp c
        pSym    SRoundKet
        return  (t, sp)

        -- The unit data constructor.
 , do   sp        <- pTokSP (KBuiltin BDaConUnit)
        return  (XCon sp dcUnit, sp)


        -- Named algebraic constructors.
 , do   (con, sp) <- pConSP
        return  (XCon sp (DaConBound con), sp)

        -- Literals.
        --   The attached type is set to Bottom for now, which needs
        --   to be filled in later by the Spread transform.
 , do   ((lit, bPrim), sp) <- pLitSP
        let Just mkLit  = contextMakeLiteralName c
        case mkLit sp lit bPrim of
         Just name -> return  (XCon sp (DaConPrim name (T.tBot T.kData)), sp)
         Nothing   -> P.unexpected "literal"

        -- Debruijn indices
 , do   (i, sp)   <- pIndexSP
        return  (XVar sp (UIx   i), sp)

        -- Variables
 , do   (var, sp) <- pVarSP
        return  (XVar sp (UName var), sp)
 ]

 <?> "a variable, constructor, or parenthesised type"


-- Alt --------------------------------------------------------------------------------------------
-- Case alternatives.
pAlt    :: (Ord n, Pretty n)
        => Context n -> Parser n (Alt SourcePos n)
pAlt c
 = do   p       <- pPat c
        pSym    SArrowDashRight
        x       <- pExp c
        return  $ AAlt p x


-- Patterns.
pPat    :: (Ord n, Pretty n)
        => Context n -> Parser n (Pat n)
pPat c
 = P.choice
 [      -- Wildcard Pattern: _
   do   pSym    SUnderscore
        return  $ PDefault

        -- LIT
 , do   --  The attached type is set to Bottom for now, which needs
        --  to be filled in later by the Spread transform.
        ((lit, bPrim), sp) <- pLitSP
        let Just mkLit  = contextMakeLiteralName c
        case mkLit sp lit bPrim of
         Just nLit      -> return  $ PData (DaConPrim nLit (T.tBot T.kData)) []
         _              -> P.unexpected "literal"

        -- Unit
 , do   pTok    (KBuiltin BDaConUnit)
        return  $ PData  dcUnit []

        -- CON BIND BIND ...
 , do   nCon    <- pCon
        bs      <- liftM concat $ P.many (pTermBinds c)
        return  $ PData (DaConBound nCon) bs]


-- Type Binds in patterns can have no kind annotations
-- or can have an annotation if the whole thing is in parens.
pTypeBinds  :: (Ord n, Pretty n)
            => Context n -> Parser n [Bind n]
pTypeBinds c
 = P.choice
        -- Plain binder.
 [ do   bs      <- P.many1 pBinder
        return  [T.makeBindFromBinder b (T.tBot T.kData) | b <- bs]

        -- Binder with kind, wrapped in parens.
 , do   pSym SRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pKind c
        pSym SRoundKet
        return  [T.makeBindFromBinder b t | b <- bs]
 ]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pTermBinds  :: (Ord n, Pretty n)
            => Context n -> Parser n [Bind n]
pTermBinds c
 = P.choice
        -- Plain binder.
 [ do   bs      <- P.many1 pBinder
        return  [T.makeBindFromBinder b (T.tBot T.kData) | b <- bs]

        -- Binder with type, wrapped in parens.
 , do   pSym SRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pSym SRoundKet
        return  [T.makeBindFromBinder b t | b <- bs]
 ]


-- Parameters of an abstraction.
pParams  :: (Ord n, Pretty n)
         => Context n -> Parser n [Param n]
pParams c
 = P.choice
        -- Plain binder.
 [ do   bs      <- P.many1 pBinder
        return  [MTerm (T.makeBindFromBinder b (T.tBot T.kData)) | b <- bs]

        -- Binder with type, wrapped in parens.
 , do   pSym SRoundBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pSym SRoundKet
        return  [MTerm (T.makeBindFromBinder b t) | b <- bs]

        -- Binder with type, wrapped in braces.
 , do   pSym SBraceBra
        bs      <- P.many1 pBinder
        pTok (KOp ":")
        t       <- pType c
        pSym SBraceKet
        return  [MImplicit (T.makeBindFromBinder b t) | b <- bs]
 ]


-- Bindings ---------------------------------------------------------------------------------------
-- | Parse some `Lets`, also returning the source position where they
--   started.
pLetsSP :: (Ord n, Pretty n)
        => Context n
        -> Parser n ([Lets SourcePos n], SourcePos)
pLetsSP c
 = P.choice
    [ -- non-recursive let.
      do sp       <- pTokSP (KKeyword ELet)
         P.choice
          -- Multiple bindings in braces
          [ do   pSym SBraceBra
                 lets    <- P.sepEndBy1 (pLetBinding c) (pSym SSemiColon)
                 pSym SBraceKet
                 return ([LLet b x | (b, x) <- lets], sp)

          -- A single binding without braces.
          , do   (b1, x1)  <- pLetBinding c
                 return ([LLet b1 x1], sp)
          ]

      -- recursive let.
    , do sp       <- pTokSP (KKeyword ELetRec)
         P.choice
          -- Multiple bindings in braces
          [ do   pSym SBraceBra
                 lets    <- P.sepEndBy1 (pLetBinding c) (pSym SSemiColon)
                 pSym SBraceKet
                 return ([LRec lets], sp)

          -- A single binding without braces.
          , do   ll      <- pLetBinding c
                 return ([LRec [ll]], sp)
          ]

      -- Private region binding.
      --   private BINDER+ (with { BINDER : TYPE ... })? in EXP
    , do sp     <- pTokSP (KKeyword EPrivate)

         -- new private region names.
         brs    <- P.manyTill pBinder
                $  P.try $ P.lookAhead $ P.choice
                        [ pTok (KKeyword EIn)
                        , pTok (KKeyword EWith) ]

         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs

         -- witness types.
         r      <- pLetWits c bs Nothing
         return ([r], sp)

      -- Extend an existing region.
      --   extend BINDER+ using TYPE (with { BINDER : TYPE ...})? in EXP
    , do sp     <- pTokSP (KKeyword EExtend)

         -- parent region
         t      <- pType c
         pTok (KKeyword EUsing)

         -- new private region names.
         brs    <- P.manyTill pBinder
                $  P.try $ P.lookAhead
                         $ P.choice
                                [ pTok (KKeyword EUsing)
                                , pTok (KKeyword EWith)
                                , pTok (KKeyword EIn) ]

         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs

         -- witness types
         r      <- pLetWits c bs (Just t)
         return ([r], sp)
    ]


pLetWits
        :: (Ord n, Pretty n)
        => Context n
        -> [Bind n] -> Maybe (Type n)
        -> Parser n (Lets SourcePos n)

pLetWits c bs mParent
 = P.choice
    [ do   pKey EWith
           pSym SBraceBra
           wits    <- P.sepBy (P.choice
                        [ -- Named witness binder.
                          do  b    <- pBinder
                              pTok (KOp ":")
                              t    <- pTypeApp c
                              return  $ T.makeBindFromBinder b t

                          -- Ambient witness binding, use for capabilities.
                        , do  t    <- pTypeApp c
                              return  $ BNone t ])
                      (pSym SSemiColon)
           pSym SBraceKet
           return (LPrivate bs mParent wits)

    , do   return (LPrivate bs mParent [])
    ]


-- | A binding for let expression.
pLetBinding
        :: (Ord n, Pretty n)
        => Context n
        -> Parser n ( Bind n
                    , Exp SourcePos n)
pLetBinding c
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok    (KOp ":")
                t       <- pType c
                pSym    SEquals
                xBody   <- pExp c

                return  $ (T.makeBindFromBinder b t, xBody)


         , do   -- Non-function binding with no type signature.
                -- This form can't be used with letrec as we can't use it
                -- to build the full type sig for the let-bound variable.
                --  BINDER = EXP
                pSym    SEquals
                xBody   <- pExp c
                let t   = T.tBot T.kData
                return  $ (T.makeBindFromBinder b t, xBody)


         , do   -- Binding using function syntax.
                ps      <- liftM concat
                        $  P.many (pBindParamSpec c)

                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound
                        -- variable.
                        --   BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                        pTok (KOp ":")
                        tBody   <- pType c
                        sp      <- pSym SEquals
                        xBody   <- pExp c

                        let x   = expOfParams sp ps xBody
                        let t   = funTypeOfParams c ps tBody
                        return  (T.makeBindFromBinder b t, x)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given
                        -- parameter types.
                        --  BINDER PARAM1 PARAM2 .. PARAMN = EXP
                 , do   sp      <- pSym SEquals
                        xBody   <- pExp c

                        let x   = expOfParams sp ps xBody
                        let t   = T.tBot T.kData
                        return  (T.makeBindFromBinder b t, x) ]
         ]


-- Stmt -------------------------------------------------------------------------------------------
data Stmt n
        = StmtBind  SourcePos (Bind n) (Exp SourcePos n)
        | StmtMatch SourcePos (Pat n)  (Exp SourcePos n) (Exp SourcePos n)
        | StmtNone  SourcePos (Exp SourcePos n)


-- | Parse a single statement.
pStmt   :: (Ord n, Pretty n)
        => Context n -> Parser n (Stmt n)
pStmt c
 = P.choice
 [ -- BINDER = EXP ;
   -- We need the 'try' because a VARIABLE binders can also be parsed
   --   as a function name in a non-binding statement.
   --
   P.try $
    do  br      <- pBinder
        sp      <- pSym SEquals
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
        sp      <- pSym SArrowDashLeft
        x1      <- pExp c
        pTok (KKeyword EElse)
        x2      <- pExp c
        return  $ StmtMatch sp p x1 x2

        -- EXP
 , do   x               <- pExp c
        return  $ StmtNone (annotOfExp x) x
 ]


-- | Parse some statements.
pStmts  :: (Ord n, Pretty n)
        => Context n -> Parser n (Exp SourcePos n)
pStmts c
 = do   stmts   <- P.sepEndBy1 (pStmt c) (pSym SSemiColon)
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

