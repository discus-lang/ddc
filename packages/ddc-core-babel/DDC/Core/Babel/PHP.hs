-- | Convert Disciple Core to PHP code
module DDC.Core.Babel.PHP where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Pretty
import DDC.Type.DataDef                 
import qualified DDC.Core.Tetra.Prim as T

import Data.Maybe (isNothing)

phpOfModule
        :: (Show a)
        => Module a T.Name
        -> Doc
phpOfModule mm
 = let ds = phpOfDataDefs $ moduleDataDefsLocal mm
       xs = phpOfExp      $ moduleBody          mm
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
      , text "$" <> bare_name name <> text "_new" <+> text " = DDC::curry(function" <> parenss (map var_name_t args) <+> text "{ return new " <+> bare_name name <> parenss (map var_name_t args) <> text "; }, " <> text (show (length args)) <> text ");"
      ]

phpOfExp
        :: (Show a)
        => Exp a T.Name
        -> Doc
phpOfExp xx
 = case xx of
    XVar _ v
     -> var_name_u v

    XCon _ DaConUnit
     -> text "1"
    XCon _ (DaConPrim n _t)
     -> sanitise_prim n
    XCon _ (DaConBound n)
     -> text "$" <> bare_name n <> text "_new"

    XLAM _ _ x
     -> phpOfExp x
    XLam a b x
     -> text "DDC::curry(function(" <> var_name_b b <> text ")/* Lam " <+> text (show a) <+> text "*/ {" <+> phpOfExp x <+> text "; }, 1)"

    XApp _ f x
     | (f',xs) <- takeXApps1 f x
     , xs'     <- noTypes xs
     -> phpOfExp f' <> parenss (map phpOfExp xs')

    XLet a lets x
     -> vcat
         [ text "/* Let " <> text (show a) <> text " */"
         , phpOfLets lets
         , (case x of XLet _ _ _ -> text ""
                      _          -> text "return ")
           <> phpOfExp x
         ]

    XCase a x alts
     -> vcat
         -- TODO
         [ text "/* Case " <> text (show a) <> text " */"
         , text "$SCRUT = " <> phpOfExp x <> text ";"
         , phpOfAlts "SCRUT" alts
         ]
    _
     -> error "No can do"

phpOfLets
        :: (Show a)
        => Lets a T.Name
        -> Doc
phpOfLets lets
 = case lets of
    LLet b x
     | Just (bs, f) <- takeXLamFlags x
     , bs' <- filter (not.fst) bs
     -> vcat
         [ makeFunction (Just b) bs f
         , var_name_b b <> text " = DDC::curry(" <> bare_name_b b <> text ", " <> text (show (length bs')) <> text ");" ]
     | otherwise
     -> var_name_b b <> text " = " <> phpOfExp x <> text ";" <> line

    LRec ((b,x):bxs)
     -> phpOfLets (LLet b x) <> line <> phpOfLets (LRec bxs)
    LRec []
     -> text ""
    
    _
     -> error "phpOfLets: no private or withregion"

phpOfAlts
        :: (Show a)
        => String
        -> [Alt a T.Name]
        -> Doc
phpOfAlts scrut alts
 = go alts
 where
  go []
   = text ""
  go (AAlt (PData dc bs) x : as)
   = vcat
      [ text "if (" <> cond dc <> text ") {"
      , indent 4 (grabfields bs)
      , indent 4 (phpOfExp x) <> text ";"
      , text " }"
      , case as of []    -> text ""
                   (_:_) -> text "else" <> go as
      ]

  go (AAlt PDefault x : _)
   = vcat
      [ text "{"
      , indent 4 (phpOfExp x) <> text ";"
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
 <> text " { "
 <> line
 <> indent 4 (phpOfExp x)
 <> line
 <> text " }"

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
 | T.NameLitUnboxed nn <- n
 = sanitise_prim nn
 | T.NameLitNat i <- n
 = text (show i)
 | otherwise
 = ppr n

string_of :: T.Name -> Doc
-- TODO
string_of n = text $ show $ show $ ppr n

parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs
