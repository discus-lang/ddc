
module DDC.Core.Transform.ANormal
    (anormalise)
where
import DDC.Core.Exp
import qualified DDC.Type.Compounds as T
import qualified DDC.Type.Universe as U
import qualified DDC.Core.Transform.LiftX as L

import qualified Data.Map as Map

-- **** Recording arities of known values
-- So we can try to create apps to fully apply 

-- I did have these as Maybe Int, but I think for our purposes 0==Nothing is fine
type Arities n = (Map.Map n Int, [Int])

arEmpty :: Ord n => Arities n
arEmpty = (Map.empty, [])

arExtends :: Ord n => Arities n -> [(Bind n, Int)] -> Arities n
arExtends arity exts = foldl go arity exts
 where	go (named,anon) (BNone _t,   _)    = (named,anon)
	go (named,anon) (BAnon _t,   a)    = (named, a:anon)
	go (named,anon) (BName n _t, a) = (Map.insert n a named, anon)

arGet :: Ord n => Arities n -> Bound n -> Int
-- TODO unsafe ix
arGet (_named, anon) (UIx ix _)	  = anon !! ix
arGet (named, _anon) (UName n _)  = named Map.! n
-- TODO Get a primitive's arity from its type
-- Actually I don't think it really matters:
--   if it's overapplied, it's a type error,
--   if it's underapplied, there's nothing we can do.
-- Assuming no higher order primitives.
arGet (_named,_anon) (UPrim _ _)  = 100

-- **** Finding arities of expressions etc

arityOfExp :: Ord n => Exp a n -> Int
arityOfExp (XLam _ b e)
    -- only count data binders
    | isComp $ U.universeOfType (T.typeOfBind b)
    = 1 + arityOfExp e
 where
    -- TODO I don't understand why these are spec universe
    isComp (Just U.UniverseSpec) = True
    isComp _                     = False
arityOfExp (XLam _ _ e)
    = arityOfExp e
arityOfExp _
    = 0

-- ha! we don't know anything about their values.
-- but we need to record them as 0 anyway (shadowing, de bruijn)
aritiesOfPat :: Ord n => Pat n -> [(Bind n, Int)]
aritiesOfPat PDefault = []
aritiesOfPat (PData _b bs) = zip bs (repeat 0)


-- **** Actually converting to a-normal form
anormal :: Ord n => Arities n -> Exp a n -> [Exp a n] -> Exp a n
anormal ar (XApp _ lhs rhs) args
 =  -- normalise applicand and record arguments
    let args' = anormal ar rhs [] : args in
    -- descend into lhs, remembering all args
    anormal ar lhs args'

anormal ar x args
 =  let x' = go x in
    -- if there are no args, we're done
    case args of
	[] -> x'
	_  -> -- there are arguments. we must apply them.
	    makeLets ar x args
 where
    -- helper for descent
    down ars e = anormal (arExtends ar ars) e []

    -- we know x isn't an app.
    go (XApp{}) = error "ANormal.anormal: impossible XApp!"

    -- leafy ones
    go (XVar{}) = x
    go (XCon{}) = x
    go (XType{}) = x
    go (XWitness{}) = x

    go (XLam a b e) =
	XLam a b (down [(b,0)] e)

    -- non-recursive let
    go (XLet a (LLet m b le) re) =
	let le' = down [] le in
	let re' = down [(b, arityOfExp le')] re in
	XLet a (LLet m b le') re'

    -- recursive let
    go (XLet a (LRec lets) re) =
	let bs = map fst lets in
	let es = map snd lets in
	let ars= zip bs (map arityOfExp es) in
	let es'= map (down ars) es in
	let re'= down ars re in
	XLet a (LRec $ zip bs es') re' 

    -- letregion, just make sure we record bindings with dummy val
    go (XLet a (LLetRegion b bs) re) =
	let ars = zip bs (repeat 0) in
	XLet a (LLetRegion b bs) (down ars re)

    -- I don't think a withregion should ever show up...
    go (XLet a (LWithRegion b) re) =
	XLet a (LWithRegion b) (down [] re)

    go (XCase a e alts) =
	let e' = down [] e in
	let alts' = map (\(AAlt pat ae) -> AAlt pat (down (aritiesOfPat pat) ae)) alts in
	XCase a e' alts'

    go (XCast a c e) =
	XCast a c (down [] e)

anormalise x = anormal arEmpty x []

isVar :: Ord n => Exp a n -> Bool
isVar (XVar{}) = True
isVar (XCon{}) = True
isVar (XType{}) = True
isVar (XWitness{}) = True
-- nah..
--isVar (XLam{}) = True
isVar _ = False
	
makeLets ar f0 args = go 0 f0 (findArity f0) args []
 where
    tBot = T.tBot T.kData

    -- sending arity of f to this is a hack because we should really be building up ar ctx?
    go i f _arf []  acc = mkApps i 0 $ acc ++ [f]
    -- f is fully applied, and we *do* have arguments left to add
    go i f arf (x:xs) acc | length acc >= arf
     =  XLet (annotOf x) (LLet LetStrict (BAnon tBot) (mkApps i 0 $ acc ++ [f]))
            (go i (XVar (annotOf x) $ UIx 0 tBot) 1 (x:xs) [])
    -- application to variable, don't bother binding
    go i f arf (x:xs) acc | isVar x
     =  go i f arf xs (x:acc)
    -- create binding
    go i f arf (x:xs) acc
     =  XLet (annotOf x) (LLet LetStrict (BAnon tBot) (L.liftX i x))
	    (go (i+1) f arf xs (x:acc))
    
    mkApps _ _ [] = error "ANormal.makeLets.mkApps: impossible!"
    mkApps l _ [x] | isVar x	= (L.liftX l x)
    mkApps _ i [x]		= XVar (annotOf x) $ UIx i tBot
    mkApps l i (x:xs) | isVar x = XApp (annotOf x) (mkApps l i xs) (L.liftX l x)
    mkApps l i (x:xs)           = XApp (annotOf x) (mkApps l (i+1) xs) (XVar (annotOf x) $ UIx i tBot)

    findArity (XVar _ b) = max (arGet ar b) 1
    findArity x          = max (arityOfExp x) 1

{-
let f = \f. \g. \x. f (g x) in
let up = \n. addInt [:R0# R0# R0#:] n (1 [R0#] ()) in
f up up (1 [R0#] ())
==>
let f = \f. \g. \x. f (g x) in
let up = \n. let ^ = 1 [R0#] () in addInt [:R0# R0# R0#:] n ^0 in
let ^ = f up up in
let ^ = 1 [R0#] () in
^1 ^0
-}

{-
[x, y, z] ->
(let ^=x in
    (let ^=y in
	(let ^=z in
	    (^2 ^1) ^0)))
-}

-- does this exist elsewhere? ought it?
annotOf :: Exp a n -> a
annotOf (XVar a _) = a
annotOf (XCon a _) = a
annotOf (XApp a _ _) = a
annotOf (XLam a _ _) = a
annotOf (XLet a _ _) = a
annotOf (XCase a _ _) = a
annotOf (XCast a _ _) = a
annotOf (XType{}) = error "DDC.Core.Transform.ANormal.annotOf: XType"
annotOf (XWitness{}) = error "DDC.Core.Transform.ANormal.annotOf: XWitness"

{-
let ap = \x. \y. x y in
(ap ap) (\x. x) (\x. x)

let ap = \x. \y. x y in
let ^ = ap ap in
let ^ = (\x. x) in
let ^ = (\x. x) in
(^2 ^1) ^0
-}
