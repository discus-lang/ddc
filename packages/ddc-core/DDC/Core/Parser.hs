-- | Core language parser.
module DDC.Core.Parser
        ( module DDC.Base.Parser
        , module DDC.Core.Parser.Param
        , module DDC.Core.Parser.Witness
        , pModule
        , pExp)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Parser.Witness
import DDC.Core.Parser.Param
import DDC.Core.Parser.Tokens
import DDC.Core.Parser.Base
import DDC.Base.Pretty
import DDC.Base.Parser                  ((<?>))
import DDC.Type.Parser                  (pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Parser        as T
import qualified Data.Map               as Map
import Control.Monad.Error


-- Module ---------------------------------------------------------------------
pModule :: (Ord n, Pretty n) 
        => Parser n (Module () n)
pModule 
 = do   pTok KModule
        name    <- pModuleName

        -- exprots { SIG;+ }
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

        -- LET;+
        lts     <- P.sepBy1 pLets (pTok KIn)

        -- TODO: make having duplicate names in the imports 
        --       or exports list a parse error, so we never build a bad map. 
        return  $ ModuleCore
                { moduleName            = name
                , moduleExportKinds     = Map.empty
                , moduleExportTypes     = Map.fromList tExports
                , moduleImportKinds     = Map.empty
                , moduleImportTypes     = Map.fromList tImports
                , moduleLets            = lts }


-- | Parse a type signature.
pTypeSig :: Ord n => Parser n (n, Type n)        
pTypeSig
 = do   var     <- pVar
        pTok KColonColon
        t       <- T.pType
        return  (var, t)


-- | Parse the type signature of an imported variable.
pImportTypeSpec 
        :: (Ord n, Pretty n) 
        => Parser n (n, (QualName n, Type n))

pImportTypeSpec 
 = do   qn      <- pQualName
        pTok KWith
        n       <- pName
        pTok KColonColon
        t       <- T.pType
        return  (n, (qn, t))
        


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
                        bs'     <- P.many1 T.pBinder
                        pTok KColon
                        t       <- T.pType
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
                        bs'     <- P.many1 T.pBinder
                        pTok KColon
                        t       <- T.pType
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


        -- withregion CON in EXP
 , do   pTok KWithRegion
        n       <- pVar
        pTok KIn
        x       <- pExp
        let u   = UName n (T.tBot T.kRegion)
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
        t       <- T.pType
        pTok KSquareKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastWeakenEffect t) x


        -- weakclo [TYPE] in EXP
 , do   pTok KWeakClo
        pTok KSquareBra
        t       <- T.pType
        pTok KSquareKet
        pTok KIn
        x       <- pExp
        return  $ XCast () (CastWeakenClosure t) x


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
        t       <- T.pType 
        pTok KSquareKet
        return  [XType t]

        -- [: TYPE0 TYPE0 ... :]
 , do   pTok KSquareColonBra
        ts      <- P.many1 T.pTypeAtom
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
        
        -- Named constructors
 , do   con     <- pCon
        return  $ XCon () (UName con (T.tBot T.kData)) 

        -- Literals
 , do   lit     <- pLit
        return  $ XCon () (UName lit (T.tBot T.kData))

        -- Debruijn indices
 , do   i       <- T.pIndex
        return  $ XVar () (UIx   i   (T.tBot T.kData))

        -- Variables
 , do   var     <- pVar
        return  $ XVar () (UName var (T.tBot T.kData)) 
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
        return  $ PData (UName nLit (T.tBot T.kData)) []

        -- CON BIND BIND ...
 , do   nCon    <- pCon 
        bs      <- P.many pBindPat
        return  $ PData (UName nCon (T.tBot T.kData)) bs]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pBindPat :: Ord n => Parser n (Bind n)
pBindPat 
 = P.choice
        -- Plain binder.
 [ do   b       <- T.pBinder
        return  $ T.makeBindFromBinder b (T.tBot T.kData)

        -- Binder with type, wrapped in parens.
 , do   pTok KRoundBra
        b       <- T.pBinder
        pTok KColon
        t       <- T.pType
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
      --   letregion BINDER with { BINDER : TYPE ... } in EXP
      --   letregion BINDER in EXP
    , do pTok KLetRegion
         br      <- T.pBinder
         let b   = T.makeBindFromBinder br T.kRegion
         P.choice 
          [ do   pTok KWith
                 pTok KBraceBra
                 wits    <- P.sepBy
                            (do  w       <- pVar
                                 pTok KColon
                                 t       <- T.pTypeApp
                                 return  (BName w t))
                            (pTok KSemiColon)
                 pTok KBraceKet
                 return (LLetRegion b wits)

          , do   return (LLetRegion b [])
          ]
    ]

-- | A binding for let expression.
pLetBinding :: Ord n => Parser n (LetMode n, Bind n, Exp () n)
pLetBinding 
 = do   b       <- T.pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok KColon
                t       <- T.pType
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
                        tBody   <- T.pType
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
 = do   b       <- T.pBinder

        P.choice
         [ do   -- Binding with full type signature.
                --  BINDER : TYPE = EXP
                pTok KColon
                t       <- T.pType
                pTok KEquals
                xBody   <- pExp

                return  $ (T.makeBindFromBinder b t, xBody) 


         , do   -- Binding using function syntax.
                --  BINDER PARAM1 PARAM2 .. PARAMN : TYPE = EXP
                ps      <- liftM concat 
                        $  P.many pBindParamSpec 
        
                pTok KColon
                tBody   <- T.pType
                let t   = funTypeOfParams ps tBody

                pTok KEquals
                xBody   <- pExp
                let x   = expOfParams () ps xBody

                return  (T.makeBindFromBinder b t, x) ]

