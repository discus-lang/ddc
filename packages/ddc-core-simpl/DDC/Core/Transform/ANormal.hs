
module DDC.Core.Transform.ANormal
    (anormalise)
where
import DDC.Core.Exp
import qualified DDC.Type.Exp as T
import qualified DDC.Type.Compounds as T
import qualified DDC.Core.Transform.LiftX as L

import qualified Data.Map as Map

-- **** Recording arities of known values
-- So we can try to create apps to fully apply 

-- | Arities of known bound variables.
-- We need to track everything even if it's not a function to keep indices correct.
-- Just use zero for unknown/irrelevant
type Arities n = (Map.Map n Int, [Int])

-- | Empty arities context
arEmpty :: Ord n => Arities n
arEmpty = (Map.empty, [])

-- | Extend map with multiple bindings and their arities
arExtends :: Ord n => Arities n -> [(Bind n, Int)] -> Arities n
arExtends arity exts = foldl go arity exts
 where	go (named,anon) (BNone _t,   _)    = (named,anon)
	go (named,anon) (BAnon _t,   a)    = (named, a:anon)
	go (named,anon) (BName n _t, a) = (Map.insert n a named, anon)

-- | Look up a binder's arity
arGet :: Ord n => Arities n -> Bound n -> Int
arGet (_named, anon) (UIx ix _)	  = anon !! ix
arGet (named, _anon) (UName n _)  = named Map.! n
-- Get a primitive's arity from its type
arGet (_named,_anon) (UPrim _ t)  = arityOfType t

-- **** Finding arities of expressions etc

-- | Count all the arrows and foralls, ignoring any effects
-- We can be sure that primitives don't effect until they're fully applied
arityOfType :: Ord n => Type n -> Int
arityOfType (T.TForall _ t)
 =  1 + arityOfType t
arityOfType t
 =  let (args, _) = T.takeTFunArgResult t in
    length args

-- | Find arity of an expression. Count lambdas, use type for primitives
arityOfExp :: Ord n => Exp a n -> Int
-- Counting all binders, because they all correspond to XApps.
arityOfExp (XLam _ _ e)
    = 1 + arityOfExp e
arityOfExp (XLAM _ _ e)
    = 1 + arityOfExp e
-- Find primitive's constructor's arities from type,
-- we might need to do this for user defined constructors too.
arityOfExp (XCon _ (UPrim _ t))
    = arityOfType t
-- Anything else we'll need to apply one at a time
arityOfExp _
    = 0

-- | Retrieve binders from case pattern, so we can extend the arity context.
-- We don't know anything about their values, so record as 0.
aritiesOfPat :: Ord n => Pat n -> [(Bind n, Int)]
aritiesOfPat PDefault = []
aritiesOfPat (PData _b bs) = zip bs (repeat 0)


-- **** Actually converting to a-normal form

-- | Recursively transform expression into a-normal
anormal :: Ord n
	=> Arities n	-- ^ environment, arities of bound variables
	-> Exp a n	-- ^ expression to transform
	-> [(Exp a n,a)]-- ^ arguments being applied to current expression
	-> Exp a n

-- Application: just record argument and descend into function
anormal ar (XApp a lhs rhs) args
 =  -- normalise rhs and add to arguments
    let args' = (anormal ar rhs [], a) : args in
    -- descend into lhs, remembering all args
    anormal ar lhs args'

-- Anything other than application: if we're applied to arguments add bindings,
-- otherwise just recurse.
anormal ar x args
 =  let x' = go x in
    case args of
	-- if there are no args, we're done
	[] -> x'
	-- there are arguments. we must apply them.
	_  -> makeLets ar x' args
 where
    -- helper for descent
    down ars e = anormal (arExtends ar ars) e []

    -- we know x isn't an app.
    go (XApp{}) = error "DDC.Core.Transform.ANormal.anormal: impossible XApp!"

    -- leafy ones
    go (XVar{}) = x
    go (XCon{}) = x
    go (XType{}) = x
    go (XWitness{}) = x

    -- lambdas
    go (XLAM a b e) =
	XLAM a b (down [(b,0)] e)
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

    -- withregion: I don't think this should ever show up.
    go (XLet a (LWithRegion b) re) =
	XLet a (LWithRegion b) (down [] re)

    -- case
    go (XCase a e alts) =
	let e' = down [] e in
	let alts' = map (\(AAlt pat ae) -> AAlt pat (down (aritiesOfPat pat) ae)) alts in
	XCase a e' alts'

    -- cast
    go (XCast a c e) =
	XCast a c (down [] e)


-- | Convert an expression into a-normal form
anormalise :: Ord n => Exp a n -> Exp a n
anormalise x = anormal arEmpty x []

-- | Check if an expression needs a binding, or if it's simple enough to be applied as-is
isNormal :: Ord n => Exp a n -> Bool
-- Trivial expressions
isNormal (XVar{}) = True
isNormal (XCon{}) = True
isNormal (XType{}) = True
isNormal (XWitness{}) = True
-- Casts are ignored by code generator, so we can leave them in if their subexpression is normal
isNormal (XCast _ _ x) = isNormal x
isNormal _ = False
	
-- | Create lets for any non-trivial arguments
makeLets :: Ord n
	=> Arities n	-- ^ environment, arities of bound variables
	-> Exp a n	-- ^ function
	-> [(Exp a n,a)]-- ^ arguments being applied to current expression
	-> Exp a n
makeLets _  f0 [] = f0
makeLets ar f0 args@((_,annot):_) = go 0 (findArity f0) ((f0,annot):args) []
 where
    tBot = T.tBot T.kData

    -- out of arguments, create XApps out of leftovers
    go i _arf [] acc = mkApps i 0 acc
    -- f is fully applied and we have arguments left to add:
    --	create let for intermediate result
    go i arf ((x,a):xs) acc | length acc > arf
     =  XLet a (LLet LetStrict (BAnon tBot) (mkApps i 0 acc))
            (go i 1 ((x,a):xs) [(XVar a $ UIx 0 tBot,a)])
    -- application to variable, don't bother binding
    go i arf ((x,a):xs) acc | isNormal x
     =  go i arf xs ((x,a):acc)
    -- non-trivial argument, create binding
    go i arf ((x,a):xs) acc
     =  XLet a (LLet LetStrict (BAnon tBot) (L.liftX i x))
	    (go (i+1) arf xs ((x,a):acc))
    
    -- fold list into applications
    -- can't create empty app
    mkApps _ _ []
     = error "DDC.Core.Transform.ANormal.makeLets.mkApps: impossible empty list"

    -- single element - this is the function
    mkApps l _ [(x,_)] | isNormal x
     = L.liftX l x
    mkApps _ i [(_,a)]
     = XVar a $ UIx i tBot

    -- apply this argument and recurse
    mkApps l i ((x,a):xs) | isNormal x
     = XApp a (mkApps l i xs) (L.liftX l x)
    mkApps l i ((_,a):xs)
     = XApp a (mkApps l (i+1) xs) (XVar a $ UIx i tBot)

    findArity (XVar _ b) = max (arGet ar b) 1
    findArity x          = max (arityOfExp x) 1

