
module DDC.Core.Transform.ANormal
    (anormalise,arGet)
where
import DDC.Core.Exp
import qualified DDC.Type.Compounds as T
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
-- Get a primitive's arity from its type
arGet (_named,_anon) (UPrim _ t)  = arityOfType t 

-- **** Finding arities of expressions etc

-- TODO Need to count arrows in data universe...
-- But only until first arrow with effect?
arityOfType :: Ord n => Type n -> Int
arityOfType _ = 0

arityOfExp :: Ord n => Exp a n -> Int
arityOfExp (XLam _ _ e)	= 1 + arityOfExp e
arityOfExp _		= 0

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

isNormal :: Ord n => Exp a n -> Bool
isNormal (XVar{}) = True
isNormal (XCon{}) = True
isNormal (XType{}) = True
isNormal (XWitness{}) = True
isNormal (XLam{}) = True
isNormal _ = False
	
makeLets _ar f args = go 0 (f:args)
 where
    tBot = T.tBot T.kData

    go lift [] = goApps lift 0 $ reverse (f:args)
    go i (x:xs) | isNormal x = go i xs
    go i (x:xs) = 
	XLet (annotOf x) (LLet LetStrict (BAnon tBot) (L.liftX i x))
	    (go (i+1) xs)
    
    goApps _ _ [] = error "ANormal.makeLets.goApps: impossible!"
    goApps l _ [x] | isNormal x	= (L.liftX l x)
    goApps _ i [x]		= XVar (annotOf x) $ UIx i tBot
    goApps l i (x:xs) | isNormal x= XApp (annotOf x) (goApps l i xs) (L.liftX l x)
    goApps l i (x:xs)             = XApp (annotOf x) (goApps l (i+1) xs) (XVar (annotOf x) $ UIx i tBot)

{-
[x, y, z] ->
(let ^=x in
    (let ^=y in
	(let ^=z in
	    (^2 ^1) ^0)))
-}

-- todo...
annotOf _ = undefined

{-
let ap = \x. \y. x y in
(ap ap) (\x. x) (\x. x)

let ap = \x. \y. x y in
let ^ = ap ap in
let ^ = (\x. x) in
let ^ = (\x . x) in
(^2 ^1) ^0
-}
