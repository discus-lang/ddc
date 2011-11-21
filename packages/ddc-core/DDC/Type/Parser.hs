
module DDC.Type.Parser
        ( Parser
        , runWrapParser
        , pType)
where
import DDC.Type.Exp
import DDC.Type.Parser.Tokens
import Data.Functor.Identity
import Text.Parsec
import Control.Monad
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Sum           as TS


-- 
type Parser k n a
        =  (Show k, Eq k)
        => ParsecT [k] (ParseState k n) Identity a


data ParseState k n
        = ParseState
        { stateTokens           :: Tokens k n
        , stateTokenShow        :: k -> String
        , stateTokenPos         :: k -> SourcePos
        , stateFileName         :: String }
        

-- | Run a type parser,
--   where the tokens are just wrapped strings.
runWrapParser
        :: (Eq k, Show k, Ord n)
        => (k -> String)           -- ^ Show a token.
        -> (k -> SourcePos)        -- ^ Take the source position of a token.
        -> (k -> String -> n)      -- ^ Convert a string to a variable name.
        -> String                  -- ^ File name for error messages.
        -> Parser k n a            -- ^ Parser to run.
        -> [k]                     -- ^ Tokens to parse.
        -> Either ParseError a

runWrapParser tokenShow tokenPos makeName fileName parser 
 = runParser parser
        ParseState 
        { stateTokens           = liftTokens tokenShow makeName tokenStrings
        , stateTokenShow        = tokenShow
        , stateTokenPos         = tokenPos
        , stateFileName         = fileName }
        fileName


---------------------------------------------------------------------------------------------------
-- | Top level parser for types.
pType   :: Ord n => Parser k n (Type n)
pType   = pType4


-- Foralls.
pType4 :: Ord n => Parser k n (Type n)
pType4
 = do   choice
         [ -- Universal quantification.
           -- [v11 v12 ... v1n : T2, v21 v22 ... v2n : T2]. T3
           do   pTok tSquareBra
                vsk     <- sepBy1 
                            (do vs      <- many1 pVar 
                                pTok tColon
                                k       <- pType2
                                return  $ (vs, k))
                            (pTok tComma)
                pTok tSquareKet
                pTok tDot

                body    <- pType3

                return  $ foldr TForall body 
                        $ [ BName v k   | (vs, k) <- vsk
                                        , v       <- vs]
          
           -- Body type
         , do   pType2]

-- Sums
pType3 :: Ord n => Parser k n (Type n)
pType3 
 = do   t1      <- pType2
        choice 
         [ -- Type sums.
           -- T2 + T3
           do   pTok tPlus
                t2      <- pType3
                return  $ TSum $ TS.fromList (TBot T.sComp) [t1, t2]
                
         , do   return t1 ]


-- Functions
pType2 :: Ord n => Parser k n (Type n)
pType2
 = do   t1      <- pType1
        choice 
         [ -- T1 -> T2
           do   pTok tTypeFun
                t2      <- pType2
                return  $ t1 T.->> t2

           -- T1 -(T0 T0)> t2
         , do   pTok tTypeFunBra
                eff     <- pType0
                clo     <- pType0
                pTok tTypeFunKet
                t2      <- pType2
                return  $ T.tFun t1 t2 eff clo

           -- T1 ~> T2
         , do   pTok tKindFun
                t2      <- pType2
                return  $ TApp (TApp (TCon TConKindFun) t1) t2

           -- Body type
         , do   return t1 ]


-- Applications
pType1 :: Ord n => Parser k n (Type n)
pType1  
 = do   (t:ts)  <- many1 pType0
        return  $  foldl TApp t ts


-- Atomics
pType0 :: Ord n => Parser k n (Type n)
pType0  = choice
        -- (TYPE2) and (->)
        [ do    pTok tRoundBra
                choice
                 [ do   t       <- pType2
                        pTok tRoundKet
                        return t 

                 , do   pTok tTypeFun
                        pTok tRoundKet
                        return (TCon $ TConType $ TyConBuiltin TyConFun)
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



---------------------------------------------------------------------------------------------------
token'  :: Stream s Identity k 
        => (Tokens k n -> k -> Maybe b)
        -> ParsecT s (ParseState k n) Identity b
token' f
 = do   state   <- getState
        token   (stateTokenShow state)
                (stateTokenPos  state)
                (f (stateTokens state))


-- | Accept a token from the table and return the given value.
pToken :: (Tokens k n -> k -> Bool) -> t -> Parser k n t
pToken f t = pTok f >> return t


-- | Accept a token from the table.
pTok     :: (Tokens k n -> k -> Bool) -> Parser k n ()
pTok f  = token' 
        $ \toks t -> if f toks t 
                        then Just ()
                        else Nothing


-- | Parse a builtin named tycon.
pTyConBuiltin :: Parser k n (TyCon n)
pTyConBuiltin   = token' tTyConBuiltin


-- | Parse a user defined named tycon.
pTyConUser :: Parser k n (TyCon n)
pTyConUser      = token' tTyConUser


-- | Parse a variable.
pVar :: Parser k n n
pVar            = token' tVar

