

module DDC.Type.Parser where
import DDC.Type.Exp
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Pos
import Control.Monad
import Data.Char
import qualified DDC.Type.Compounds     as T

data Tokens k
        = Tokens
        { tRoundBra     :: k
        , tRoundKet     :: k
        , tSquareBra    :: k
        , tSquareKet    :: k
        , tColon        :: k
        , tDot          :: k
        , tSortComp     :: k
        , tSortProp     :: k
        , tKindValue    :: k
        , tKindRegion   :: k
        , tKindEffect   :: k
        , tKindClosure  :: k
        , tKindWitness  :: k
        , tKindFun      :: k
        , tTypeFun      :: k
        , tTypeFunBra   :: k
        , tTypeFunKet   :: k
        , tBotEffect    :: k
        , tBotClosure   :: k
        , tTyConBuiltin :: k -> Maybe (TyCon k k)
        , tTyConUser    :: k -> Maybe (TyCon k k)
        , tVar          :: k -> Maybe k }

tokenStrings :: Tokens String
tokenStrings
        = Tokens
        { tRoundBra     = "("
        , tRoundKet     = ")"
        , tSquareBra    = "["
        , tSquareKet    = "]"
        , tColon        = ":"
        , tDot          = "."
        , tSortComp     = "**"
        , tSortProp     = "@@"
        , tKindValue    = "*"
        , tKindRegion   = "%"
        , tKindEffect   = "!"
        , tKindClosure  = "$"
        , tKindWitness  = "@"
        , tKindFun      = "~>"
        , tTypeFun      = "->"
        , tTypeFunBra   = "-("
        , tTypeFunKet   = ")>"
        , tBotEffect    = "!0"
        , tBotClosure   = "$0"
        , tTyConBuiltin = readTyConBuiltin
        , tTyConUser    = readTyConUser 
        , tVar          = readVar }


readTyConBuiltin :: String -> Maybe (TyCon k k)
readTyConBuiltin ss
 = case ss of
        "Read"          -> Just TyConRead
        "DeepRead"      -> Just TyConDeepRead
        "Write"         -> Just TyConWrite
        "DeepWrite"     -> Just TyConDeepWrite
        "Alloc"         -> Just TyConAlloc
        "Free"          -> Just TyConFree
        "DeepFree"      -> Just TyConDeepFree
        "Const"         -> Just TyConConst
        "DeepConst"     -> Just TyConDeepConst
        "Mutable"       -> Just TyConMutable
        "DeepMutable"   -> Just TyConDeepMutable
        "Lazy"          -> Just TyConLazy
        "HeadLazy"      -> Just TyConHeadLazy
        "Direct"        -> Just TyConDirect
        "Pure"          -> Just TyConPure
        "Empty"         -> Just TyConEmpty
        _               -> Nothing


-- | Read a named, user defined TyCon.
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
--   TODO: This doesn't reject junk on the end of the name.
readTyConUser :: String -> Maybe (TyCon String String)
readTyConUser (s:ss)
        | isUpper s     = Just (TyConData (s:ss) (TBot T.sComp))
        | otherwise     = Nothing

-- | Read a named, user defined variable.
--   TODO: This doesn't reject junk at the end of the name.
readVar :: String -> Maybe String
readVar str@(s:ss)
        | isLower s     = Just str
        | otherwise     = Nothing

runParserOfStrings
        :: Parser String a
        -> [String]
        -> Either ParseError a

runParserOfStrings parser
 = runParser parser 
        tokenStrings
        "foo"
        

---------------------------------------------------------------------------------------------------
type Parser k a
        =  (Show k, Eq k)
        => ParsecT [k] (Tokens k) Identity a


---------------------------------------------------------------------------------------------------
-- | Functions.
pType3 :: Parser k (Type k k)
pType3
 = do   choice
         [ -- [v : T2]. T3
           do   pTok tSquareBra
                v       <- pVar
                pTok tColon
                k       <- pType2
                pTok tSquareKet
                pTok tDot
                body    <- pType3
                return  $ TForall (BName v k) body
          
           -- Body type
         , do   pType2]


pType2 :: Parser k (Type k k)
pType2
 = do   t1      <- pType1
        choice 
         [ -- t1 -> t2
           do   pTok tTypeFun
                t2      <- pType2
                return  $ TApp (TApp (TCon (TConType TyConFun)) t1) t2

           -- k1 ~> k2
         , do   pTok tKindFun
                t2      <- pType2
                return  $ TApp (TApp (TCon TConKindFun) t1) t2

           -- Body type
         , do   return t1 ]


-- | Applications.
pType1 :: Parser k (Type k k)
pType1  
 = do   (t:ts)  <- many1 pType0
        return  $  foldl TApp t ts


-- | Constructors and atomic things.
pType0 :: Parser k (Type k k)
pType0  = choice
        -- (TYPE2) and (->)
        [ do    pTok tRoundBra
                choice
                 [ do   t       <- pType2
                        pTok tRoundKet
                        return t 

                 , do   pTok tTypeFun
                        pTok tRoundKet
                        return (TCon $ TConType TyConFun)
                 ]

        -- Named type constructors
        , do    tc      <- pTyConBuiltin
                return  $ TCon (TConType tc)

        , do    tc      <- pTyConUser
                return  $ TCon (TConType tc)

        -- Symbolic constructors.
        , do    pToken tSortComp    (TCon $ TConSort SoConComp)
        , do    pToken tSortProp    (TCon $ TConSort SoConProp) 
        , do    pToken tKindValue   (TCon $ TConKind KiConData)
        , do    pToken tKindRegion  (TCon $ TConKind KiConRegion) 
        , do    pToken tKindEffect  (TCon $ TConKind KiConEffect) 
        , do    pToken tKindClosure (TCon $ TConKind KiConClosure) 
        , do    pToken tKindWitness (TCon $ TConKind KiConWitness) 
            
        -- Bottoms.
        , do    pToken tBotEffect  (TBot T.kEffect)
        , do    pToken tBotClosure (TBot T.kClosure)
      
        -- Bound occurrence of a variable.
        -- We don't know the kind of this variable yet, so fill in the field with the bottom
        -- element of computation kinds. This isn't really part of the language, but makes
        -- sense implentation-wise.
        , do    v       <- pVar
                return  $  TVar (UName v (TBot T.sComp))
        ]


--pBind :: Parser k (Bind 

---------------------------------------------------------------------------------------------------
-- | Accept a token from the table and return the given value.
pToken :: (Tokens k -> k) -> t -> Parser k t
pToken f t = pTok f >> return t


-- | Accept a token from the table.
pTok     :: (Tokens k -> k) -> Parser k ()
pTok f   
 = do   toks    <- getState
        token   show
                (const (newPos "foo" 0 0))
                (\t -> if f toks == t 
                                then Just () else Nothing)


-- | Parse a builtin named tycon.
pTyConBuiltin :: Parser k (TyCon k k)
pTyConBuiltin
 = do   toks    <- getState
        token   show
                (const (newPos "foo" 0 0))
                (tTyConBuiltin toks)


-- | Parse a user defined named tycon.
pTyConUser :: Parser k (TyCon k k)
pTyConUser
 = do   toks    <- getState
        token   show
                (const (newPos "foo" 0 0))
                (tTyConUser toks)

-- | Parse a variable
pVar :: Parser k k
pVar
 = do   toks    <- getState
        token   show
                (const (newPos "foo" 0 0))
                (tVar toks)

