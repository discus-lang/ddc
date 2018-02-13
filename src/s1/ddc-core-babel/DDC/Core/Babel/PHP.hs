-- | Convert Disciple Core to PHP code
module DDC.Core.Babel.PHP where
import DDC.Core.Collect
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Core.Pretty
import DDC.Type.DataDef
import qualified DDC.Core.Discus.Prim as T
import qualified DDC.Type.Env as Env

import qualified Data.Set as Set
import qualified Data.Map as Map

-- import Data.Maybe (isNothing)


phpOfModule
        :: (Show a)
        => Module a T.Name
        -> Doc
phpOfModule mm
 = let ds = phpOfDataDefs $ moduleDataDefsLocal mm
       m  = Map.fromList
          $ map arityOfImport $ moduleImportValues mm
       xs = phpOfExp       (moduleBody          mm) CTop m
   in vcat [ text "<?php"
           , ds
           , xs
           , text "?>" ]
 where
  arityOfImport (n,i)
   = case i of
     ImportValueModule{}
      | Just (_,a, _) <- importValueModuleArity i
      -> (n, a)
      | otherwise
      -> (n, arityOfType' (importValueModuleType i))
     ImportValueSea{}
      -> (n, arityOfType' (importValueSeaType i))

  arityOfType' = arityOfType . eraseTForalls


phpOfDataDefs
        :: [DataDef T.Name]
        -> Doc
phpOfDataDefs ds
 = vcat $ concatMap def ds
 where
  def d
   | Just cs <- dataDefCtors d
   = map ctor cs
   | otherwise
   = []

  ctor c
   = let name = dataCtorName c
         args = map (("_"++) . show) [1..length (dataCtorFieldTypes c)]
     in vcat
      [ text "class" <+> bare_name name <+> text "{"
      , indent 4 $ vcat $
        [ text "function __construct" <> parenss (map var_name_t args) <+> text "{"
        , indent 4 $ vcat
                  $ map (\i -> obj_field_tt "this" i
                            <> text " = " <> var_name_t i <> text ";") args
        , indent 4 $ obj_field_tt "this" "tag" <+> text " = " <+> string_of name <> text ";"
        , text "}"
        ]
      , text "}"
      -- , text "$" <> bare_name name <> text "_new" <+> text "
      --         = DDC::curry(function" <> parenss (map var_name_t args)
      --                <+> text "{ return new " <+> bare_name name
      --                <> parenss (map var_name_t args) <> text "; }, "
      --                <> text (show (length args)) <> text ");"
      ]

data Context
 = CLet (Bind T.Name)
 | CRet
 | CTop
 | CExp

phpOfExp
        :: (Show a)
        => Exp a T.Name
        -> Context
        -> Map.Map T.Name Int -- ^ arities
        -> Doc
phpOfExp xx ctx m
 = case xx of
    XVar _ v
     | UName n <- v
     , Just  arity <- Map.lookup n m
     -> wrap $ text "DDC::curry(" <> bare_name n <> text ", "
                                  <> text (show arity) <> text ")"
     | UPrim p _ <- v
     -> wrap $ phpOfPrimOp p []
     | otherwise
     -> wrap $ var_name_u v

    XCon _ DaConUnit
     -> wrap $ text "1"
    XCon _ (DaConPrim n _t)
     -> wrap $ sanitise_prim n
    -- constructors must be fully applied
    XCon _ (DaConBound n)
     -> wrap $ text "new " <> bare_name n

    XLAM _ _ x
     -> phpOfExp x ctx m
    XLam a _ _
     | Just (bs, f) <- takeXLamFlags xx
     , bs' <- filter (not.fst) bs
     -> wrap $ text "DDC::curry(/* Lam " <+> text (show a)
                <+> text "*/" <+> makeFunction Nothing bs f m <> text ", "
                <> text (show (length bs')) <> text ")"

     -- (" <> var_name_b b <> text ")/* Lam " <+> text (show a)
     --   <+> text "*/ {" <+> phpOfExp x CRet <+> text " }, 1)"

{-
    XApp _ f x
     | (f',xs) <- takeXApps1 f x
     , xs'     <- noTypes xs
     , XVar _ (UName n)
               <- f'
     , Just arity <- Map.lookup n m
     -> if arity == length xs'
        then wrap $ bare_name n <> parenss (map (\arg -> phpOfExp arg CExp m) xs')
        -- todo also curry in phpOfLet (xvar)
        else wrap $ text "DDC::apply"
                <> parenss ((text "DDC::curry(" <> bare_name n <> text ", " <> text (show arity)
                <> text ")") : map (\arg -> phpOfExp arg CExp m) xs')

     | (f',xs) <- takeXApps1 f x
     , xs'     <- noTypes xs
     , XVar _ (UPrim p _)
               <- f'
     -> wrap $ phpOfPrimOp p (map (\arg -> phpOfExp arg CExp m) xs')

     | (f',xs) <- takeXApps1 f x
     , xs'     <- noTypes xs
     -> wrap $ phpOfExp f' CExp m <> parenss (map (\arg -> phpOfExp arg CExp m) xs')
-}

    XLet a lets x
     | (ldocs, m') <- phpOfLets lets ctx m
     -> vcat
         [ text "/* Let " <> text (show a) <> text " */"
         , ldocs
         , phpOfExp x ctx m'
         ]

    XCase a x alts
     -> vcat
         -- Case expressions aren't finished.
         [ text "/* Case " <> text (show a) <> text " */"
         , text "$SCRUT = " <> phpOfExp x CExp m <> text ";"
         , phpOfAlts "SCRUT" alts ctx m
         ]
    XCast _ _ x
     -> phpOfExp x ctx m
    _
     -> error ("ddc-core-babel.phpOfExp No can do: " ++ show (ppr xx))
 where
  wrap d
   = case ctx of
     -- throw away top-level expressions (the unit at the end)
     CTop       -> text ""
     CExp       -> d
     CLet (BNone _) -> d <> text ";"
     CLet b     -> var_name_b b <> text " = " <> d <> text ";"
     CRet       -> text "return " <> d <> text ";"


phpOfLets
        :: (Show a)
        => Lets a T.Name
        -> Context
        -> Map.Map T.Name Int
        -> (Doc, Map.Map T.Name Int)
phpOfLets lets ctx m
 = case lets of
    LLet b x
     | Just (bs, f) <- takeXLamFlags x
     , CTop <- ctx
     -> (makeFunction (Just b) bs f m, insertArity (b,x) m)
         -- , var_name_b b <> text " = DDC::curry(" <> bare_name_b b
         --  <> text ", " <> text (show (length bs')) <> text ");" ]
     | otherwise
     -> (phpOfExp x (CLet b) m <> line, m)

    LRec bxs
     | m' <- foldr insertArity m bxs
     -> ( foldl (<>) empty $ map (\(b,x) -> fst $ phpOfLets (LLet b x) ctx m') bxs
        , m')

    _
     -> error "ddc-core-babel.phpOfLets: no private or withregion"
 where
  insertArity (b,x) mm
     | Just (bs, _) <- takeXLamFlags x
     , BName n _<- b
     , bs' <- filter (not.fst) bs
     , CTop <- ctx
     = Map.insert n (length bs') mm
     | otherwise
     = mm

phpOfAlts
        :: (Show a)
        => String
        -> [Alt a T.Name]
        -> Context
        -> Map.Map T.Name Int
        -> Doc
phpOfAlts scrut alts ctx m
 = go alts
 where
  go []
   = text ""
  go (AAlt (PData dc bs) x : as)
   = vcat
      [ text "if (" <> cond dc <> text ") {"
      , indent 4 (grabfields bs)
      , indent 4 (phpOfExp x ctx m)
      , text " }"
      , case as of []    -> text ""
                   (_:_) -> text "else" <> go as
      ]

  go (AAlt PDefault x : _)
   = vcat
      [ text "{"
      , indent 4 (phpOfExp x ctx m)
      , text "}" ]

  cond DaConUnit
   = text "true"

  cond DaConRecord{}
   = error "ddc-core-babel: records not implemented"

  cond (DaConPrim n _t)
   = var_name_t scrut <> text " == " <> (sanitise_prim n)

  cond (DaConBound n)
   = obj_field_tt scrut "tag" <> text " == " <> string_of n

  grabfields bs
   = vcat $ zipWith grabfield bs [1 :: Int ..]
  grabfield b i
   = var_name_b b <> text " = " <> obj_field_tt scrut ("_" ++ show i) <> text ";"

makeFunction
        :: (Show a)
        => Maybe (Bind T.Name)
        -> [(Bool, Bind T.Name)]
        -> Exp a T.Name
        -> Map.Map T.Name Int
        -> Doc
makeFunction nm bs x m
 = text "function "
 <> maybe (text "") bare_name_b nm
 <> parenss (map (var_name_b.snd) $ filter (not.fst) bs)
 <> use_
 <> text " { "
 <> line
 <> indent 4 (phpOfExp x CRet m)
 <> line
 <> text " }"
 where
  fx = map var_name_u
     $ filter (\vu -> case vu of UName _ -> True ; _ -> False)
     $ Set.toList
     $ freeX Env.empty
     $ makeXLamFlags (annotOfExp x) bs x
  use_
   = case nm of
      Nothing
       | not $ null fx
       -> text " use " <> parenss fx
      _
       -> text ""

noTypes :: [Exp a T.Name] -> [Exp a T.Name]
noTypes _xs
  = error "ddc-core-babel.noTypes: fixme"
--  filter (isNothing.takeXWitness)
--  $ filter (isNothing.takeXType) xs


-- todo strip out
bare_name :: T.Name -> Doc
bare_name = ppr

bare_name_b :: Bind T.Name -> Doc
bare_name_b (BName n _) = bare_name n
bare_name_b (BNone   _) = text "__NONE__"
bare_name_b _ = error "ddc-core-babel.bare_name: Only named vars allowed"

var_name_b :: Bind T.Name -> Doc
var_name_b b = text "$" <> bare_name_b b

var_name_u :: Bound T.Name -> Doc
var_name_u (UName n) = text "$" <> bare_name n
var_name_u (UIx _) = error "ddc-core-babel.var_name: Only named vars allowed"
var_name_u (UPrim n _) = sanitise_prim n

var_name_t :: String -> Doc
var_name_t n = text "$" <> text n

obj_field :: Doc -> Doc -> Doc
obj_field n m = text "$" <> n <> text "->" <> m

obj_field_tt :: String -> String -> Doc
obj_field_tt n m = obj_field (text n) (text m)

sanitise_prim :: T.Name -> Doc
sanitise_prim n
 | T.NameLitBool True <- n
 = text "true"
 | T.NameLitBool False <- n
 = text "false"
 | T.NameLitNat i <- n
 = text (show i)
 | T.NameLitInt i <- n
 = text (show i)
 | T.NameLitSize i <- n
 = text (show i)
 | T.NameLitWord i _ <- n
 = text (show i)
 | T.NameLitFloat i _ <- n
 = text (show i)
 | T.NameLitTextLit t <- n
 = text (show t)

 | T.NamePrimArith p _ <- n
 = text ("DDC::" ++ show p)
 -- = text ("DDC::curry(DDC::" ++ show p ++ ", DDC::" ++ show p ++ "_arity)")

 | T.NameLitUnboxed nn <- n
 = sanitise_prim nn

 | otherwise
 = ppr n

-- String conversion isn't finished.
string_of :: T.Name -> Doc
string_of n = text $ show $ show $ ppr n

parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs

phpOfPrimOp :: T.Name -> [Doc] -> Doc
phpOfPrimOp op args
 | Just (ty, s) <- getOp
 = case (ty, args) of
    (Infix, [l,r])
     -> text "(" <> l <+> text s <+> r <> text ")"
    (Prefix, [r])
     -> text "(" <> text s <+> r <> text ")"
    (Suffix, [l])
     -> text "(" <> l <+> text s <> text ")"
    _
     -> fallback

 | otherwise
 = fallback
 where
  fallback
   = text "DDC::apply"
       <> parenss ((text "DDC::curry(" <> sanitise_prim op <> text ", "
                        <> sanitise_prim op <> text "_arity)") : args)
  getOp
   = go operators
  go []
   = Nothing
  go ((o,t,s):os)
   | o == op
   = Just (t,s)
   | otherwise
   = go os

data OpType
 = Infix
 | Prefix
 | Suffix
operators :: [(T.Name,OpType,String)]
operators
 = lmap (flip T.NamePrimArith False) ariths
 where
  lmap f = map (\(n,o,s) -> (f n, o, s))
  ariths
   = [(T.PrimArithNeg,  Prefix, "-")
     ,(T.PrimArithAdd,  Infix,  "+")
     ,(T.PrimArithSub,  Infix,  "-")
     ,(T.PrimArithMul,  Infix,  "*")
     ,(T.PrimArithDiv,  Infix,  "/")
     ,(T.PrimArithMod,  Infix,  "%")
     ,(T.PrimArithRem,  Infix,  "%")
     ,(T.PrimArithEq,   Infix,  "==")
     ,(T.PrimArithNeq,  Infix,  "!=")
     ,(T.PrimArithGt,   Infix,  ">")
     ,(T.PrimArithGe,   Infix,  ">=")
     ,(T.PrimArithLt,   Infix,  "<")
     ,(T.PrimArithLe,   Infix,  "<=")
     ,(T.PrimArithAnd,  Infix,  "&&")
     ,(T.PrimArithOr,   Infix,  "||")
     ]
