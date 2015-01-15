-- | Convert Disciple Core to PHP code
module DDC.Core.Babel.PHP where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Pretty
import DDC.Type.DataDef                 

import Data.Maybe (isNothing)

phpOfModule
        :: (Show a, Pretty n, Eq n)
        => Module a n
        -> Doc
phpOfModule mm
 = let ds = phpOfDataDefs $ moduleDataDefsLocal mm
       xs = phpOfExp      $ moduleBody          mm
   in vcat [ text "<?php"
           , ds
           , xs
           , text "?>" ]


phpOfDataDefs
        :: (Pretty n, Eq n)
        => [DataDef n]
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
      ]

phpOfExp
        :: (Show a, Pretty n, Eq n)
        => Exp a n
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
     -> text "new" <+> bare_name n

    XLAM _ _ x
     -> phpOfExp x
    XLam a b x
     -> text "function(" <> var_name_b b <> text ")/* Lam " <+> text (show a) <+> text "*/ {" <+> phpOfExp x <+> text "; }"

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
        :: (Show a, Pretty n, Eq n)
        => Lets a n
        -> Doc
phpOfLets lets
 = case lets of
    LLet b x
     | Just (bs, f) <- takeXLamFlags x
     -> vcat
         [ makeFunction (Just b) bs f
         , var_name_b b <> text " = " <> bare_name_b b <> text ";" ]
     | otherwise
     -> var_name_b b <> text " = " <> phpOfExp x <> text ";" <> line

    LRec ((b,x):bxs)
     -> phpOfLets (LLet b x) <> line <> phpOfLets (LRec bxs)
    LRec []
     -> text ""
    
    _
     -> error "phpOfLets: no private or withregion"

phpOfAlts
        :: (Show a, Pretty n, Eq n)
        => String
        -> [Alt a n]
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
        :: (Show a, Pretty n, Eq n)
        => Maybe (Bind n)
        -> [(Bool, Bind n)]
        -> Exp a n
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

noTypes :: [Exp a n] -> [Exp a n]
noTypes xs
  = filter (isNothing.takeXWitness)
  $ filter (isNothing.takeXType) xs


-- todo strip out
bare_name :: Pretty n => n -> Doc
bare_name = ppr

bare_name_b :: Pretty n => Bind n -> Doc
bare_name_b (BName n _) = bare_name n
bare_name_b (BNone   _) = text "__NONE__"
bare_name_b _ = error "Only named vars allowed"

var_name_b :: Pretty n => Bind n -> Doc
var_name_b b = text "$" <> bare_name_b b

var_name_u :: Pretty n => n -> Doc
var_name_u n = text "$" <> bare_name n

var_name_t :: String -> Doc
var_name_t n = text "$" <> text n

obj_field :: Doc -> Doc -> Doc
obj_field n m = text "$" <> n <> text "->" <> m

obj_field_tt :: String -> String -> Doc
obj_field_tt n m = obj_field (text n) (text m)

sanitise_prim :: Pretty n => n -> Doc
sanitise_prim n
 | n' == "True#"
 = text "true"
 | n' == "False#"
 = text "false"
 | otherwise
 = text n'
 where
  n' = show (ppr n)

string_of :: Pretty n => n -> Doc
-- TODO
string_of n = text $ show $ show $ ppr n

parenss :: [Doc] -> Doc
parenss xs = encloseSep lparen rparen (comma <> space) xs
