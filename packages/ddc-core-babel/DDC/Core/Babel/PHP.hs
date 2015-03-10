-- | Convert Disciple Core to PHP code
module DDC.Core.Babel.PHP where
import DDC.Core.Compounds
import DDC.Core.Collect
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Pretty
import DDC.Type.DataDef                 
import qualified DDC.Core.Tetra.Prim as T
import qualified DDC.Type.Env as Env
import qualified Data.Set as Set

import Data.Maybe (isNothing)

phpOfModule
        :: (Show a)
        => Module a T.Name
        -> Doc
phpOfModule mm
 = let ds = phpOfDataDefs $ moduleDataDefsLocal mm
       xs = phpOfExp       (moduleBody          mm) CTop
   in vcat [ text "<?php"
           , ds
           , xs
           , text "?>" ]


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
        , indent 4 $ vcat $ map (\i -> obj_field_tt "this" i <> text " = " <> var_name_t i <> text ";") args
        , indent 4 $ obj_field_tt "this" "tag" <+> text " = " <+> string_of name <> text ";"
        , text "}"
        ]
      , text "}"
      -- , text "$" <> bare_name name <> text "_new" <+> text " = DDC::curry(function" <> parenss (map var_name_t args) <+> text "{ return new " <+> bare_name name <> parenss (map var_name_t args) <> text "; }, " <> text (show (length args)) <> text ");"
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
        -> Doc
phpOfExp xx ctx
 = case xx of
    XVar _ v
     -> wrap $ var_name_u v

    XCon _ DaConUnit
     -> wrap $ text "1"
    XCon _ (DaConPrim n _t)
     -> wrap $ sanitise_prim n
    -- constructors must be fully applied
    XCon _ (DaConBound n)
     -> wrap $ text "new " <> bare_name n

    XLAM _ _ x
     -> phpOfExp x ctx
    XLam a _ _
     | Just (bs, f) <- takeXLamFlags xx
     , bs' <- filter (not.fst) bs
     -> wrap $ text "DDC::curry(/* Lam " <+> text (show a) <+> text "*/" <+> makeFunction Nothing bs f <> text ", " <> text (show (length bs')) <> text ")"
     
     -- (" <> var_name_b b <> text ")/* Lam " <+> text (show a) <+> text "*/ {" <+> phpOfExp x CRet <+> text " }, 1)"

    XApp _ f x
     | (f',xs) <- takeXApps1 f x
     , xs'     <- noTypes xs
     -> wrap $ phpOfExp f' CExp <> parenss (map (flip phpOfExp CExp) xs')

    XLet a lets x
     -> vcat
         [ text "/* Let " <> text (show a) <> text " */"
         , phpOfLets lets ctx
         , phpOfExp x ctx
         ]

    XCase a x alts
     -> vcat
         -- TODO
         [ text "/* Case " <> text (show a) <> text " */"
         , text "$SCRUT = " <> phpOfExp x CExp <> text ";"
         , phpOfAlts "SCRUT" alts ctx
         ]
    XCast _ _ x
     -> phpOfExp x ctx
    _
     -> error ("No can do: " ++ show (ppr xx))
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
        -> Doc
phpOfLets lets ctx
 = case lets of
    LLet b x
     | Just (bs, f) <- takeXLamFlags x
     , bs' <- filter (not.fst) bs
     , CTop <- ctx
     -> vcat
         [ makeFunction (Just b) bs f
         , var_name_b b <> text " = DDC::curry(" <> bare_name_b b <> text ", " <> text (show (length bs')) <> text ");" ]
     | otherwise
     -> phpOfExp x (CLet b) <> line

    LRec ((b,x):bxs)
     -> phpOfLets (LLet b x) ctx <> line <> phpOfLets (LRec bxs) ctx
    LRec []
     -> text ""
    
    _
     -> error "phpOfLets: no private or withregion"

phpOfAlts
        :: (Show a)
        => String
        -> [Alt a T.Name]
        -> Context
        -> Doc
phpOfAlts scrut alts ctx
 = go alts
 where
  go []
   = text ""
  go (AAlt (PData dc bs) x : as)
   = vcat
      [ text "if (" <> cond dc <> text ") {"
      , indent 4 (grabfields bs)
      , indent 4 (phpOfExp x ctx)
      , text " }"
      , case as of []    -> text ""
                   (_:_) -> text "else" <> go as
      ]

  go (AAlt PDefault x : _)
   = vcat
      [ text "{"
      , indent 4 (phpOfExp x ctx)
      , text "}" ]

  cond DaConUnit
   = text "true"
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
        -> Doc
makeFunction nm bs x
 = text "function " 
 <> maybe (text "") bare_name_b nm
 <> parenss (map (var_name_b.snd) $ filter (not.fst) bs)
 <> use_
 <> text " { "
 <> line
 <> globals_
 <> indent 4 (phpOfExp x CRet)
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
  globals_
   = case nm of
      Just _ 
       | not $ null fx
       -> text " global " <> encloseSep empty empty (comma <> space) fx <> text ";" <> line
      _
       -> text ""

noTypes :: [Exp a T.Name] -> [Exp a T.Name]
noTypes xs
  = filter (isNothing.takeXWitness)
  $ filter (isNothing.takeXType) xs


-- todo strip out
bare_name :: T.Name -> Doc
bare_name = ppr

bare_name_b :: Bind T.Name -> Doc
bare_name_b (BName n _) = bare_name n
bare_name_b (BNone   _) = text "__NONE__"
bare_name_b _ = error "Only named vars allowed"

var_name_b :: Bind T.Name -> Doc
var_name_b b = text "$" <> bare_name_b b

var_name_u :: Bound T.Name -> Doc
var_name_u (UName n) = text "$" <> bare_name n
var_name_u (UIx _) = error "Only named vars allowed"
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
 | T.NameLitString t <- n
 = text (show t)

 | T.NamePrimArith p <- n
 = text ("DDC::" ++ show p)
 -- = text ("DDC::curry(DDC::" ++ show p ++ ", DDC::" ++ show p ++ "_arity)")

 | T.NameLitUnboxed nn <- n
 = sanitise_prim nn

 | otherwise
 = ppr n

string_of :: T.Name -> Doc
-- TODO
string_of n = text $ show $ show $ ppr n

parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs
