{-# LANGUAGE TypeFamilies #-}

-- | Parser for Source Tetra expressions.
module DDC.Source.Tetra.Parser.Exp
        ( pExp
        , pExpAppSP
        , pExpAtomSP
        , pLetsSP,      pClauseSP
        , pType
        , pTypeApp
        , pTypeAtomSP)
where
import DDC.Source.Tetra.Parser.Type
import DDC.Source.Tetra.Parser.Witness
import DDC.Source.Tetra.Parser.Base
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim            as S
import DDC.Core.Lexer.Tokens
import Control.Monad.Except
import Data.Maybe
import qualified DDC.Control.Parser     as P
import qualified Data.Text              as Text


type SP = SourcePos


-- Exp --------------------------------------------------------------------------------------------
pExp   :: Parser Exp
pExp = fmap snd pExpWhereSP


-- An expression that may have a trailing where clause.
pExpWhereSP :: Parser (SP, Exp)
pExpWhereSP 
 = do   (sp1, xx) <- pExpAppSP

        P.choice
         [ do   -- x where GROUP
                sp      <- pKey EWhere
                pSym SBraceBra
                cls     <- liftM (map snd)
                        $  P.sepEndBy1 pClauseSP (pSym SSemiColon)
                pSym SBraceKet
                return  (sp1, XWhere sp xx cls)

         , do   return  (sp1, xx) ]


-- An application of a function to its arguments,
-- or a plain expression with no arguments.
pExpAppSP :: Parser (SP, Exp)
pExpAppSP
  = do  (spF, (xF, asArg)) <- pExpAppsSP
        case asArg of
                []     -> return (spF, xF)
                _      -> return (spF, XDefix spF (RTerm xF : asArg))

  <?> "an expression or application"


-- An application of a function to its arguments,
-- or a plan expression with no arguments.
pExpAppsSP :: Parser (SP, (Exp, [Arg]))
pExpAppsSP
 = do   (spF, xFun) <- pExpFrontSP
        xsArg       <- pExpArgsSP pExpAtomSP
        return (spF, (xFun, xsArg))


-- A list of arguments.
pExpArgsSP :: Parser (SP, Exp) -> Parser [Arg]
pExpArgsSP pX
 = P.choice
 [ do   -- After an infix operator we allow the next expression
        -- to be a compound expression rather than an atom.
        --  This allows code like (f x $ λy. g x y) as in Haskell.
        (UName txOp, sp) <- pBoundNameOpSP
        xsMore           <- pExpArgsSP pExpFrontSP
        return  (RTerm (XInfixOp  sp (Text.unpack txOp)) : xsMore)

        -- Some arguments.
 , do   (_, xsArg)       <- pExpArgsSpecSP pX
        xsMore           <- pExpArgsSP     pExpAtomSP
        return  (xsArg ++ xsMore)

        -- No more arguments.
 , do   return  []
 ]


-- Comp, Witness or Spec arguments.
pExpArgsSpecSP :: Parser (SP, Exp) -> Parser (SP, [Arg])
pExpArgsSpecSP pX
 = P.choice
        -- [Type]
 [ do   sp      <- pSym SSquareBra
        t       <- pType
        pSym    SSquareKet
        return  (sp, [RType t])

        -- [: Type0 Type0 ... :]
 , do   sp      <- pSym SSquareColonBra
        ts      <- fmap (fst . unzip) $ P.many1 pTypeAtomSP
        pSym    SSquareColonKet
        return  (sp, [RType t | t <- ts])

        -- {^ Witness }
 , do   sp      <- pSym SBraceBra
        P.choice
         [ do   _       <- pSym SHat
                w       <- pWitness
                pSym SBraceKet
                return  (sp, [RWitness w])

                -- { Exp }
         , do   x       <- pExp
                pSym    SBraceKet
                return  (sp, [RImplicit x])
         ]
               
        -- Exp0
 , do   (sp, x)  <- pX
        return  (sp, [RTerm x])
 ]
 <?> "a type, witness or expression argument"


-- | Parse a compound Source Tetra expression.
--   The first token determines the form of the expression.
pExpFrontSP :: Parser (SP, Exp)
pExpFrontSP
 = P.choice

        -- Level-0 lambda abstractions
        --  \(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
        --  \x1 x2 : Type. Exp
        --  \x1 x2. Exp
 [ do   sp      <- P.choice 
                        [ pSym SLambda
                        , pSym SBackSlash ]
        pts     <- fmap concat $ P.many1 pTermParam
        pSym    SArrowDashRight
        xBody   <- pExp
        return  (sp, XAnnot sp 
                        $ foldr (\(ps, p, mt) -> XAbsPat sp ps p mt) 
                                xBody pts)


        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : Type) (y1 y2 ... : Type) ... . Exp
 , do   sp      <- P.choice 
                        [ pSym SBigLambda
                        , pSym SBigLambdaSlash ]

        bmts    <- P.choice
                [ fmap concat $ P.many1
                   $ do pSym SRoundBra
                        bs'     <- P.many1 pBind
                        pTok (KOp ":")
                        t       <- pType
                        pSym SRoundKet
                        return  [(b, Just t)  | b <- bs']

                , do    bs'     <- P.many1 pBind
                        return  [(b, Nothing) | b <- bs']
                ]

        pSym    SArrowDashRight
        xBody   <- pExp
        return  (sp, XAnnot sp 
                        $ foldr (\(b, mt) x -> XAbs (MType b mt) x) 
                                xBody bmts)


        -- let expression
 , do   (lts, sp) <- pLetsSP
        pTok    (KKeyword EIn)
        x2      <- pExp
        return  (sp, XAnnot sp $ XLet lts x2)


        -- Sugar for a let-expression.
        --  do { Stmt;+ }
 , do   sp      <- pKey EDo
        pSym    SBraceBra
        xx      <- pStmts
        pSym    SBraceKet
        return  (sp, xx)


        -- case Exp of { Alt;+ }
 , do   sp      <- pKey ECase
        x       <- pExp
        pKey    EOf
        pSym    SBraceBra
        alts    <- P.sepEndBy1 pAltCase (pSym SSemiColon)
        pSym    SBraceKet
        return  (sp, XAnnot sp $ XCase x alts)


        -- match { | EXP = EXP | EXP = EXP ... }
        --  Sugar for cascaded case expressions case-expression.
 , do   sp      <- pKey EMatch
        pSym SBraceBra

        gxs     <- liftM (map (AAltMatch . snd))
                $  P.sepEndBy1  (pGuardedExpSP (pSym SEquals)) 
                                (pSym SSemiColon)

        let xError
                = makeXErrorDefault 
                        (Text.pack    $ sourcePosSource sp) 
                        (fromIntegral $ sourcePosLine   sp)

        pSym SBraceKet
        return  (sp, XAnnot sp $ XMatch sp gxs xError)


 , do   -- if-then-else
        --  Sugar for a case-expression.
        sp      <- pTokSP (KKeyword EIf)
        x1      <- pExp
        pTok (KKeyword EThen)
        x2      <- pExp
        pTok (KKeyword EElse)
        x3      <- pExp 
        return  (sp, XAnnot sp $ XCase x1 
                        [ AAltCase PTrue    [GExp x2]
                        , AAltCase PDefault [GExp x3]])


        -- weakeff [Type] in Exp
 , do   sp      <- pTokSP (KKeyword EWeakEff)
        pSym    SSquareBra
        t       <- pType
        pSym    SSquareKet
        pKey    EIn
        x       <- pExp
        return  (sp, XAnnot sp $ XCast (CastWeakenEffect t) x)


        -- purify Witness in Exp
 , do   sp      <- pKey EPurify
        w       <- pWitness
        pTok (KKeyword EIn)
        x       <- pExp
        return  (sp, XAnnot sp $ XCast (CastPurify w) x)


        -- box Exp
 , do   sp      <- pKey EBox
        x       <- pExp
        return  (sp, XAnnot sp $ XCast CastBox x)


        -- run Exp
 , do   sp      <- pKey ERun
        x       <- pExp
        return  (sp, XAnnot sp $ XCast CastRun x)

        -- ATOM
 , do   pExpAtomSP
 ]
 <?> "an expression"


pTermParam :: Parser [(ParamSort, Pat, Maybe Type)]
pTermParam
 = P.choice
 [ P.try $ do 
         pSym SRoundBra
         ps   <- P.many1 pPat
         pTok (KOp ":")
         t    <- pType
         pSym SRoundKet
         return  [(MSTerm, p, Just t) | p <- ps]

 , P.try $ do
         pSym SBraceBra
         ps   <- P.many1 pPat
         pTok (KOp ":")
         t    <- pType
         pSym SBraceKet
         return  [(MSImplicit, p, Just t) | p <- ps]

 , do    ps      <- P.many1 pPatAtom
         return  [(MSTerm, p, Nothing) | p <- ps]
 ]


-- | Parse a variable, constructor or parenthesised expression,
--   also returning source position.
pExpAtomSP :: Parser (SP, Exp)
pExpAtomSP
 = P.choice
 [      -- Named algebraic constructors.
   do   (con, sp)       <- pDaConBoundNameSP
        return  (sp, XCon  (DaConBound con))


        -- Literals.
        --  We just fill-in the type with a hole for now, and leave it to
        --  We also set the literal as being algebraic, which may not be
        --  true (as for Floats). The spreader also needs to fix this.
 , do   (lit, sp)       <- pDaConBoundLitSP
        return  (sp, XCon (DaConPrim lit (TVar UHole)))


        -- Fragment specific primitive names.
 , do   (nPrim, sp)     <- pPrimValSP
        return  (sp, XFrag nPrim)

 , do   (u, sp)         <- pBoundNameSP 
        case u of 
         UName tx
          |  Text.pack "project" == tx
          -> do pSym SRoundBra
                n    <- fmap fst $ pVarNameSP
                pSym SRoundKet
                pSym SHash
                return (sp, XPrim (PProject n))

         _      -> return (sp, XVar u)

{-
        -- Named variables.
 , do   (u,  sp)        <- pBoundNameSP

        -- Detect names of primitives in the ambient calculus.
        if      u == (UName $ Text.pack "project#") 
                then return (sp, XPrim PProject)
        else if u == (UName $ Text.pack "shuffle#")
                then return (sp, XPrim PShuffle)
        else if u == (UName $ Text.pack "combine#")
                then return (sp, XPrim PCombine)

        else return (sp, XVar u)
-}

        -- Debruijn indices
 , do   (u, sp)         <- pBoundIxSP
        return  (sp, XVar u)


       -- Primitive record data constructor.
       --  like (x,y,z)# 
 , P.try $ do
        sp      <- pSym SRoundBra
        ns      <- fmap (map fst) $ P.sepBy pVarNameSP (pSym SComma)
        pSym SRoundKet
        pSym SHash
        return  (sp, XCon (DaConRecord ns))


 -- Full application of a record data constructor.
 --   like (x = e1, y = e2, z = e3)
 , P.try $ do
        sp       <- pSym SRoundBra

        (nsField, tsField, xsField) 
         <- fmap unzip3
          $ P.sepBy 
                (do (n, _)  <- pVarNameSP
                    P.choice
                     [ do _      <- pTokSP (KOp ":")
                          t      <- pType
                          _      <- pSym SEquals
                          x      <- pExp
                          return (n, t, x)

                     , do _      <- pSym SEquals
                          x      <- pExp
                          return (n, TVar UHole, x)])
                (pSym SComma)

        pSym SRoundKet
        let xRecord = XCon (DaConRecord nsField)
        return  ( sp
                , makeXApps xRecord (map RType tsField ++ map RTerm xsField))


        -- The syntax for the nullary record type constructor '()#' overlaps
        -- with that of the unit data construtor '()', so try the former first.
 , P.try $ do
        sp      <- pTokSP (KBuiltin BDaConUnit)
        pSym SHash
        return  (sp, XCon (DaConRecord []))


        -- ( Exp2 )
 , do   pSym SRoundBra
        (sp, t)  <- pExpWhereSP
        pSym SRoundKet
        return  (sp, t)


        -- Infix operator used as a variable.
 , do   (UName tx, sp) <- pBoundNameOpVarSP
        return  (sp, XInfixVar sp (Text.unpack tx))


        -- Infix operator used nekkid.
 , do   (UName tx, sp) <- pBoundNameOpSP
        return  (sp, XInfixOp  sp (Text.unpack tx))

  
        -- The unit data constructor.       
 , do   sp              <- pTokSP (KBuiltin BDaConUnit)
        return  (sp, XCon  dcUnit)
 ]
 <?> "a variable, constructor, or parenthesised type"


-- Alternatives -----------------------------------------------------------------------------------
-- Case alternatives.
pAltCase :: Parser AltCase
pAltCase
 = do   p       <- pPat
        P.choice
         [ do   -- Desugar case guards while we're here.
                spgxs     <- P.many1 (pGuardedExpSP (pSym SArrowDashRight))
                let gxs  = map snd spgxs
                return  $ AAltCase p gxs 
                
         , do   pSym SArrowDashRight
                x       <- pExp
                return  $ AAltCase p [GExp x] ]


-- Patterns.
pPat :: Parser Pat
pPat
 = P.choice
 [  -- Con Bind Bind ...
    do  nCon    <- pDaConBoundName 
        ps      <- P.many pPatAtom
        return  $ PData (DaConBound nCon) ps

    -- Atom
 ,  do  p       <- pPatAtom
        return  p
 ]
 <?> "a pattern"


pPatAtom :: Parser Pat
pPatAtom
 = P.choice
 [ 
        -- Primitive record pattern.
        -- like (x,y,z)# a b c
    P.try $ do
        _       <- pSym SRoundBra
        nsField <- fmap (map fst) $ P.sepBy pVarNameSP (pSym SComma)
        pSym SRoundKet
        pSym SHash
        psField <- P.many pPatAtom
        return  $ PData (DaConRecord nsField) psField


        -- Sugared record pattern.
        -- like (x = a, y = b, z = c)
 , P.try $ do
        _       <- pSym SRoundBra

        (nsField, psField) 
         <- fmap unzip
          $ P.sepBy 
                (do (n, _)  <- pVarNameSP
                    _       <- pSym SEquals
                    p       <- pPatAtom
                    return (n, p))
                (pSym SComma)

        pSym SRoundKet
        return  $ PData (DaConRecord nsField) psField

        -- ( PAT )
 , P.try $ do   
        pSym SRoundBra
        p       <- pPat
        pSym SRoundKet
        return  $ p

        -- Wildcard
        --   Try this case before the following one for binders
        --   so that '_' is parsed as the default pattern,
        --   rather than a wildcard binder.
 , do   pSym SUnderscore
        return  $ PDefault

        -- Var
 , do   b       <- pBind
        P.choice
         [ do   _       <- pSym SAt
                p       <- pPatAtom
                return  $  PAt b p

         , do   return  $  PVar b
         ]

        -- Lit
 , do   nLit    <- pDaConBoundLit
        return  $ PData (DaConPrim nLit (TBot S.KData)) []

        -- Named algebraic constructors.
 , do   nCon    <- pDaConBoundName
        return  $ PData (DaConBound nCon) []

        -- 'Unit'
 , do   pTok    (KBuiltin BDaConUnit)
        return  $ PData  dcUnit []
 ]
 <?> "a pattern"


-- Bindings ---------------------------------------------------------------------------------------
pLetsSP :: Parser (Lets, SP)
pLetsSP 
 = P.choice
    [ -- non-recursive let
      do sp       <- pKey ELet
         l        <- liftM snd $ pClauseSP
         return (LGroup [l], sp)

      -- recursive let
    , do sp       <- pKey ELetRec
         pSym SBraceBra
         ls       <- liftM (map snd)
                  $  P.sepEndBy1 pClauseSP (pSym SSemiColon)
         pSym SBraceKet
         return (LGroup ls, sp)

      -- Private region binding.
      --   private Binder+ (with { Binder : Type ... })? in Exp
    , do sp     <- pKey EPrivate
         
        -- new private region names.
         bs     <- P.manyTill pBind
                $  P.try 
                        $ P.lookAhead 
                        $ P.choice [pKey EIn, pKey EWith]
         
         -- Witness types.
         r      <- pLetWits bs Nothing
         return (r, sp)

      -- Extend an existing region.
      --   extend Binder+ using Type (with { Binder : Type ...})? in Exp
    , do sp     <- pTokSP (KKeyword EExtend)

         -- parent region
         t      <- pType
         pTok (KKeyword EUsing)

         -- new private region names.
         bs     <- P.manyTill pBind
                $  P.try $ P.lookAhead 
                         $ P.choice 
                                [ pTok (KKeyword EUsing)
                                , pTok (KKeyword EWith)
                                , pTok (KKeyword EIn) ]
         
         -- witness types
         r      <- pLetWits bs (Just t)
         return (r, sp)
    ]
    
    
pLetWits :: [Bind] -> Maybe Type -> Parser Lets
pLetWits bs mParent
 = P.choice 
    [ do   pKey EWith
           pSym SBraceBra
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
                      (pSym SSemiColon)
           pSym SBraceKet
           return (LPrivate bs mParent wits)
    
    , do   return (LPrivate bs mParent [])
    ]


-- | A binding for let expression.
pClauseSP :: Parser (SP, Clause)
pClauseSP
 = do   -- Name of the binding.
        (b, sp0) <- pBindNameSP

        P.choice
         [ do   -- Either 
                -- 1) a definition with a signature and some clauses.
                --    foo : Nat -> Nat = ...
                -- 2) foo : Nat -> Nat
                --
                _       <- pTokSP (KOp ":")
                t       <- pType
                P.choice
                 [ do   
                        gxs     <- pTermGuardedExps (pSym SEquals)
                        return  (sp0,  SLet sp0 (XBindVarMT b (Just t)) [] gxs)

                 , do   return  (sp0,  SSig sp0 b t)
                 ]

         , do   -- Non-function binding with no type signature.
                gxs     <- pTermGuardedExps (pSym SEquals)
                return  (sp0, SLet sp0 (XBindVarMT b Nothing)  [] gxs)

         , do   -- Binding using function syntax.
                ps      <- fmap concat $ P.many pParamsSP
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   Binder Param1 Param2 .. ParamN : Type = Exp
                        sp      <- pTokSP (KOp ":")
                        tBody   <- pType
                        gxs     <- pTermGuardedExps (pSym SEquals)

                        let t   = funTypeOfParams     ps tBody
                        return  (sp, SLet sp (XBindVarMT b (Just t))  ps gxs)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable.
                 , do   gxs     <- pTermGuardedExps (pSym SEquals)
                        return  (sp0, SLet sp0 (XBindVarMT b Nothing) ps gxs)
                 ]
         ]


-- Function parameters.
pParamsSP :: Parser [Param]
pParamsSP
 = P.choice
        -- Type parameter
        --   [BIND1 BIND2 .. BINDN : TYPE]
 [ do   pSym SSquareBra
        bs      <- P.many1 pBind
        pTok (KOp ":")
        t       <- pType
        pSym SSquareKet
        return  [ MType b (Just t) | b <- bs]


        -- Implicit term parameter
        --   {BIND : TYPE}
 , do   pSym  SBraceBra
        ps      <- P.choice
                [  P.try $ do
                        ps      <- P.many1 pPatAtom
                        pTok (KOp ":")
                        t       <- pType
                        return  [ MImplicit p (Just t) | p <- ps ]

                , do    p       <- pPat
                        return  [ MImplicit p Nothing ]
                ]
        pSym  SBraceKet
        return  ps


        -- Value parameter without a type annotation.
        --   This needs to come before the case for value patterns with type
        --   annotations because both records and the unit data constructor
        --   pattern start with a '('.
 , do   p       <- pPatAtom
        return  [MTerm p Nothing]


        -- Term pattern with type annotations.
        -- (BIND1 BIND2 .. BINDN : TYPE) 
 , do   pSym    SRoundBra
        ps      <- P.choice
                [  P.try $ do
                        ps      <- P.many1 pPatAtom
                        pTok (KOp ":")
                        t       <- pType
                        return  [ MTerm p (Just t) | p <- ps ]

                , do    p       <- pPat
                        return  [ MTerm p Nothing ]
                ]

        pSym    SRoundKet
        return ps
 ]
 <?> "a function parameter"


--   and the type of the body.
funTypeOfParams 
        :: [Param]      -- ^ Spec of parameters.
        -> Type         -- ^ Type of body.
        -> Type         -- ^ Type of whole function.

funTypeOfParams [] tBody        
 = tBody

funTypeOfParams (p:ps) tBody
 = case p of
        MType     b mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TApp (TCon (TyConForall k)) (TAbs b k $ funTypeOfParams ps tBody)

        MTerm     _ mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TFunExplicit k  $ funTypeOfParams ps tBody

        MImplicit  _ mt
         -> let k       = fromMaybe (TBot S.KData) mt
            in  TFunImplicit k  $ funTypeOfParams ps tBody


-- Guards -----------------------------------------------------------------------------------------
-- | Parse either the terminating char and a single expression, 
--   or some guarded expressions.
pTermGuardedExps
        :: Parser SP    -- ^ Parser for char between guards and exp
        -> Parser [GuardedExp]

pTermGuardedExps pTerm
 = P.choice
 [ do   _       <- pTerm
        xBody   <- pExp
        return  [GExp xBody]

 , do   fmap (map snd)
         $ P.many1 $ pGuardedExpSP pTerm
 ]


-- | An guarded expression,
--   like | EXP1 = EXP2.
pGuardedExpSP 
        :: Parser  SP   -- ^ Parser for char between and of guards and exp.
                        --   usually -> or =
        -> Parser  (SP, GuardedExp)

pGuardedExpSP pTermSP
 = pGuardExp (pSym SBar)

 where  pGuardExp pSepSP
         = P.choice
         [ do   sp      <- pSepSP
                g       <- pGuard
                gx      <- liftM snd $ pGuardExp (pSym SComma)
                return  (sp, GGuard g gx)

         , do   sp      <- pTermSP
                x       <- pExp
                return  (sp, GExp x) ]

        pGuard
         = P.choice 
         [ P.try $
           do   p       <- pPat
                pSym    SArrowDashLeft
                x       <- pExp
                return $ GPat p x

         , do   g       <- pExp
                return $ GPred g

         , do   pKey    EOtherwise
                return GDefault ]


-- Statements -------------------------------------------------------------------------------------
-- These are statements inside a do expression.
--   
--   A 'do' block behaves much the same way as a 'let' expression,
--   except we can write effectful statements without binding the
--   result to anything.
-- 
--   We currently desugar do expressions to let-expressions in the parser.
--

-- | Represent a statement inside a do block.
data Stmt
        -- | Let-binding or type signature.
        = StmtClause SP Clause

        -- | Case match.
        | StmtMatch  SP Pat Exp Exp

        -- | Plain statement without binding the result.
        | StmtNone   SP Exp


-- | Parse a single statement.
pStmt :: Parser Stmt
pStmt
 = P.choice
 [ -- Clause;
   -- We need the 'try' because the function and argument names at the front
   -- of a clause can also be parsed as a function application in a statement.
   P.try $ 
    do  (sp, c)  <- pClauseSP
        return  $ StmtClause sp c

   -- Pat <- Exp else Exp ;
   -- Sugar for a case-expression.
   -- We need the 'try' because the PAT can also be parsed
   --  as a function name in a non-binding statement.
 , P.try $
    do  p       <- pPat
        sp      <- pSym SArrowDashLeft
        x1      <- pExp
        pTok (KKeyword EElse)
        x2      <- pExp
        return  $ StmtMatch sp p x1 x2

        -- Exp
 , do   (sp, x) <- pExpWhereSP
        return  $ StmtNone sp x
 ]


-- | Parse some statements.
pStmts :: Parser Exp
pStmts 
 = do   -- Parse statements in the block.
        stmts   <- P.sepEndBy1 pStmt (pSym SSemiColon)
        
        -- As in Haskell, we require do blocks to end with a statement
        -- that gives the overall value.
        case makeStmts [] stmts of
         Nothing -> P.unexpected "do-block must end with a statement"
         Just x  -> return x


-- | Make an expression from some statements.
--   We collect consecutive clauses into the same clause group, 
--   so that we can define functions with multiple clauses at the top
--   level of a 'do' expression.
makeStmts :: [Clause] -> [Stmt] -> Maybe Exp
makeStmts clsAcc ss
 = let  
        -- Wrap the clauses we're carrying around the given
        -- body expression.
        dropClauses xBody
         = case clsAcc of
                []      -> xBody
                [SLet sp bmt [] [GExp x]]
                        -> XAnnot sp $ XLet (LLet bmt x) xBody
                _       -> XLet (LGroup clsAcc) xBody
   in
      case ss of
        [StmtNone _ x]    
         -> Just $ dropClauses
                 $ x

        StmtNone sp x1 : rest
         |  Just x2     <- makeStmts [] rest
         -> Just $ XAnnot sp 
                 $ dropClauses
                 $ XLet (LLet (XBindVarMT BNone Nothing) x1) x2

        StmtClause _ cl : rest
         -> case clsAcc of
                -- Start accumulating successive clauses.
                [] -> makeStmts (clsAcc ++ [cl]) rest

                (cl1 : _)
                   -- If this clause is for the same function as
                   -- the previous one then we'll collect it into
                   -- at letrec at this point.
                   |  bindOfClause cl1 == bindOfClause cl
                   -> makeStmts (clsAcc ++ [cl]) rest

                   -- Otherwise make a standard let-expression.
                _  | Just x3 <- makeStmts [] rest
                   -> case cl of
                        (SLet sp bmt [] [GExp x])
                         -> Just $ XAnnot sp 
                         $  dropClauses
                         $  XLet (LLet bmt x) x3

                        _ -> Nothing

                   | otherwise
                   -> Nothing


        StmtMatch sp p x1 x2 : rest
         |  Just x3      <- makeStmts [] rest
         -> Just $ XAnnot sp 
                 $ dropClauses
                 $ XCase x1 
                 [ AAltCase p        [GExp x3]
                 , AAltCase PDefault [GExp x2] ]

        _ -> Nothing


