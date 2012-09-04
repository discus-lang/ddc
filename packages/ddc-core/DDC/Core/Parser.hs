-- | Core language parser.
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , module DDC.Core.Parser.Param
        , module DDC.Core.Parser.Witness
        , pModule
        , pExp
        , pType
        , pTypeApp
        , pTypeAtom)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Param
import DDC.Core.Parser.Type
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Compounds
import DDC.Base.Pretty
import DDC.Base.Parser                  ((<?>))
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified Data.Map               as Map
import Control.Monad.Error


-- Module ---------------------------------------------------------------------
pModule :: (Ord n, Pretty n) 
        => Parser n (Module () n)
pModule 
 = do   pTok KModule
        name    <- pModuleName

        -- exports { SIG;+ }
        tExports 
         <- P.choice
            [do pTok KExports
                pTok KBraceBra
                sigs    <- P.sepEndBy1 pTypeSig (pTok KSemiColon)
                pTok KBraceKet
                return sigs

            ,   return []]

        -- imports { SIG;+ }
        tImports
         <- P.choice
            [do pTok KImports
                pTok KBraceBra
                specs    <- P.sepEndBy1 pImportTypeSpec (pTok KSemiColon)
                pTok KBraceKet
                return specs

            ,   return []]

        pTok KWith

        -- LET;+
        lts     <- P.sepBy1 pLets (pTok KIn)

        -- The body of the module consists of the top-level bindings wrapped
        -- around a unit constructor place-holder.
        let body = makeXLets () lts (xUnit ())

        -- TODO: make having duplicate names in the imports 
        --       or exports list a parse error, so we never build a bad map. 
        return  $ ModuleCore
                { moduleName            = name
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.fromList tExports
                , moduleImportKinds     = Map.empty
                , moduleImportTypes     = Map.fromList tImports
                , moduleBody            = body }


-- | Parse a type signature.
pTypeSig :: Ord n => Parser n (n, Type n)        
pTypeSig
 = do   var     <- pVar
        pTok KColonColon
        t       <- pType
        return  (var, t)


-- | Parse the type signature of an imported variable.
pImportTypeSpec 
        :: (Ord n, Pretty n) 
        => Parser n (n, (QualName n, Type n))

pImportTypeSpec 
 = P.choice
 [      -- Import with an explicit external name.
        -- Module.varExternal with varLocal
   do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        t       <- pType
        return  (n, (qn, t))

 , do   n       <- pName
        pTok KColonColon
        t       <- pType
        return  (n, (QualName (ModuleName []) n, t))
 ]        
        


-- Expressions ----------------------------------------------------------------
-- | Parse a core language expression.
pExp    :: Ord n => Parser n (Exp () n)
pExp 
 = P.choice
        -- Level-0 lambda abstractions
        -- \(x1 x2 ... : TYPE) (y1 y2 ... : TYPE) ... . EXP
 [ do   pTok KBackSlash

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
        return  $ foldr (XLam ()) xBody bs

        -- Level-1 lambda abstractions.
        -- /\(x1 x2 ... : TYPE) (y1 y2 ... : TYPE) ... . EXP
 , do   pTok KBigLambda

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
        return  $ foldr (XLAM ()) xBody bs


        -- let expression
 , do   lts     <- pLets
        pTok    KIn
        x2      <- pExp
        return  $ XLet () lts x2


        -- do { STMTS }
        --   Sugar for a let-expression.
 , do   pTok    KDo
        pTok    KBraceBra
        xx      <- pStmts
        pTok    KBraceKet
        return  $ xx


        -- withregion CON in EXP
 , do   pTok KWithRegion
        n       <- pVar
        pTok KIn
        x       <- pExp
        let u   = UName n
        return  $ XLet () (LWithRegion u) x


        -- case EXP of { ALTS }
 , do   pTok KCase
        x       <- pExp
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 pAlt (pTok KSemiColon)
        pTok KBraceKet
        return  $ XCase () x alts


        -- match PAT <- EXP else EXP in EXP
        --  Sugar for a case-expression.
 , do   pTok KMatch
        p       <- pPat
        pTok KArrowDashLeft
        x1      <- pExp 
        pTok KElse
        x2      <- pExp 
        pTok KIn
        x3      <- pExp
        return  $ XCase () x1 [AAlt p x3, AAlt PDefault x2]


        -- weakeff [TYPE] in EXP
 , do   pTok KWeakEff
        pTok KSquareBra
        t       <- pType
        pTok KSquareKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastWeakenEffect t) x


        -- weakclo {EXP;+} in EXP
 , do   pTok KWeakClo
        pTok KBraceBra
        xs       <- liftM concat $ P.sepEndBy1 pArgs (pTok KSemiColon)
        pTok KBraceKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastWeakenClosure xs) x


        -- purify <WITNESS> in EXP
 , do   pTok KPurify
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastPurify w) x


        -- forget <WITNESS> in EXP
 , do   pTok KForget
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastForget w) x

        -- APP
 , do   pExpApp
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Ord n => Parser n (Exp () n)
pExpApp 
  = do  x1      <- pExp0
        
        P.choice
         [ do   xs  <- liftM concat $ P.many1 pArgs
                return  $ foldl (XApp ()) x1 xs

         ,      return x1]

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgs   :: Ord n => Parser n [Exp () n]
pArgs 
 = P.choice
        -- [TYPE]
 [ do   pTok KSquareBra
        t       <- pType 
        pTok KSquareKet
        return  [XType t]

        -- [: TYPE0 TYPE0 ... :]
 , do   pTok KSquareColonBra
        ts      <- P.many1 pTypeAtom
        pTok KSquareColonKet
        return  $ map XType ts
        
        -- <WITNESS>
 , do   pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        return  [XWitness w]
                
        -- <: WITNESS0 WITNESS0 ... :>
 , do   pTok KAngleColonBra
        ws      <- P.many1 pWitnessAtom
        pTok KAngleColonKet
        return  $ map XWitness ws
                
        -- EXP0
 , do   x       <- pExp0
        return  [x]
 ]
 <?> "a type, witness or expression argument"


-- Atomics
pExp0   :: Ord n => Parser n (Exp () n)
pExp0 
 = P.choice
        -- (EXP2)
 [ do   pTok KRoundBra
        t       <- pExp
        pTok KRoundKet
        return  $ t
 
        -- The unit data constructor.       
 , do   pTok KDaConUnit
        return  $ XCon () dcUnit

        -- Named algebraic constructors.
        --  We just fill-in the type with tBot for now, and leave it to 
        --  the spreader to attach the real type.
 , do   con     <- pCon
        return  $ XCon () (mkDaConAlg con (T.tBot T.kData))

        -- Literals.
        --  We just fill-in the type with tBot for now, and leave it to
        --  the spreader to attach the real type.
        --  We also set the literal as being algebraic, which may not be
        --  true (as for Floats). The spreader also needs to fix this.
 , do   lit     <- pLit
        return  $ XCon () (mkDaConAlg lit (T.tBot T.kData))

        -- Debruijn indices
 , do   i       <- pIndex
        return  $ XVar () (UIx   i)

        -- Variables
 , do   var     <- pVar
        return  $ XVar () (UName var) 
 ]

 <?> "a variable, constructor, or parenthesised type"


-- Case alternatives.
pAlt    :: Ord n => Parser n (Alt () n)
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
pLets :: Ord n => Parser n (Lets () n)
pLets
 = P.choice
    [ -- non-recursive let.
      do pTok KLet
         (mode1, b1, x1) <- pLetBinding
         return  $ LLet mode1 b1 x1

      -- recursive let.
    , do pTok KLetRec
         P.choice
          -- Multiple bindings in braces
          [ do   pTok KBraceBra
                 lets    <- P.sepEndBy1 pLetRecBinding (pTok KSemiColon)
                 pTok KBraceKet
                 return $ LRec lets

          -- A single binding without braces.
          , do   ll      <- pLetRecBinding
                 return  $ LRec [ll]
          ]      

      -- Local region binding.
      --   letregions [BINDER] with { BINDER : TYPE ... } in EXP
      --   letregions [BINDER] in EXP
    , do pTok KLetRegions
         brs    <- P.manyTill pBinder (P.try $ P.lookAhead $ P.choice [pTok KIn, pTok KWith])
         let bs =  map (flip T.makeBindFromBinder T.kRegion) brs
         pLetWits bs
          
    , do pTok KLetRegion
         br    <- pBinder
         let b =  T.makeBindFromBinder br T.kRegion
         pLetWits [b]
         
    ]
    
    
pLetWits :: Ord n => [Bind n] -> Parser n (Lets () n)
pLetWits bs
 = P.choice 
    [ do   pTok KWith
           pTok KBraceBra
           wits    <- P.sepBy
                      (do  w       <- pVar
                           pTok KColon
                           t       <- pTypeApp
                           return  (BName w t))
                      (pTok KSemiColon)
           pTok KBraceKet
           return (LLetRegions bs wits)
    
    , do   return (LLetRegions bs [])
    ]


-- | A binding for let expression.
pLetBinding :: Ord n => Parser n (LetMode n, Bind n, Exp () n)
pLetBinding 
 = do   b       <- pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok KColon
                t       <- pType
                mode    <- pLetMode
                pTok KEquals
                xBody   <- pExp

                return  $ (mode, T.makeBindFromBinder b t, xBody) 


         , do   -- Non-function binding with no type signature.
                -- This form can't be used with letrec as we can't use it
                -- to build the full type sig for the let-bound variable.
                --  BINDER = EXP
                mode    <- pLetMode
                pTok KEquals
                xBody   <- pExp
                let t   = T.tBot T.kData
                return  $ (mode, T.makeBindFromBinder b t, xBody)


         , do   -- Binding using function syntax.
                ps      <- liftM concat 
                        $  P.many pBindParamSpec 
        
                P.choice
                 [ do   -- Function syntax with a return type.
                        -- We can make the full type sig for the let-bound variable.
                        --   BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                        pTok KColon
                        tBody   <- pType
                        mode    <- pLetMode
                        pTok KEquals
                        xBody   <- pExp

                        let x   = expOfParams () ps xBody
                        let t   = funTypeOfParams ps tBody
                        return  (mode, T.makeBindFromBinder b t, x)

                        -- Function syntax with no return type.
                        -- We can't make the type sig for the let-bound variable,
                        -- but we can create lambda abstractions with the given 
                        -- parameter types.
                        --  BINDER PARAM1 PARAM2 .. PARAMN = EXP
                 , do   mode    <- pLetMode
                        pTok KEquals
                        xBody   <- pExp

                        let x   = expOfParams () ps xBody
                        let t   = T.tBot T.kData
                        return  (mode, T.makeBindFromBinder b t, x) ]
         ]

-- | Parse a let mode specifier.
--   Only allow the lazy specifier with non-recursive bindings.
--   We don't support value recursion, so the right of all recursive
--   bindings must be explicit lambda abstractions anyway, so there's 
--   no point suspending them.
pLetMode :: Ord n => Parser n (LetMode n)
pLetMode
 = do   P.choice
                -- lazy <WITNESS>
         [ do   pTok KLazy

                P.choice
                 [ do   pTok KAngleBra
                        w       <- pWitness
                        pTok KAngleKet
                        return  $ LetLazy (Just w)
                 
                 , do   return  $ LetLazy Nothing ]

         , do   return  $ LetStrict ]


-- | Letrec bindings must have a full type signature, 
--   or use function syntax with a return type so that we can make one.
pLetRecBinding :: Ord n => Parser n (Bind n, Exp () n)
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

                pTok KEquals
                xBody   <- pExp
                let x   = expOfParams () ps xBody

                return  (T.makeBindFromBinder b t, x) ]


-- Stmts ----------------------------------------------------------------------
data Stmt n
        = StmtBind  (Bind n) (Exp () n)
        | StmtMatch (Pat n)  (Exp () n) (Exp () n)
        | StmtNone  (Exp () n)


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
        pTok    KEquals
        x1      <- pExp
        let t   = T.tBot T.kData
        let b   = T.makeBindFromBinder br t
        return  $ StmtBind b x1

   -- PAT <- EXP else EXP;
   -- Sugar for a case-expression.
   -- We need the 'try' because the PAT can also be parsed
   --  as a function name in a non-binding statement.
 , P.try $
    do  p       <- pPat
        pTok KArrowDashLeft
        x1      <- pExp 
        pTok KElse
        x2      <- pExp 
        return  $ StmtMatch p x1 x2

        -- EXP
 , do   x       <- pExp
        return  $ StmtNone x
 ]


-- | Parse some statements.
pStmts :: Ord n => Parser n (Exp () n)
pStmts
 = do   stmts   <- P.sepEndBy1 pStmt (pTok KSemiColon)
        case makeStmts stmts of
         Nothing -> P.unexpected "do-block must end with a statement"
         Just x  -> return x


-- | Make an expression from some statements.
makeStmts :: [Stmt n] -> Maybe (Exp () n)
makeStmts ss
 = case ss of
        [StmtNone x]    
         -> Just x

        StmtNone x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet () (LLet LetStrict (BNone (T.tBot T.kData)) x1) x2

        StmtBind b x1 : rest
         | Just x2      <- makeStmts rest
         -> Just $ XLet () (LLet LetStrict b x1) x2

        StmtMatch p x1 x2 : rest
         | Just x3      <- makeStmts rest
         -> Just $ XCase () x1 
                 [ AAlt p x3
                 , AAlt PDefault x2]

        _ -> Nothing
