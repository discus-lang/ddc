module DDC.Core.Transform.Flatten
        ( flatten
        , flatten1)
where
import DDC.Core.Transform.TransformX
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.LiftX
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Predicates
import Data.Functor.Identity


-- | Flatten binding structure in a thing.
--
--   Flattens nested let-expressions, 
--   and single alternative let-case expressions
--
--   * Does not change the order of evaluation.
--   * Weakly improving, will not make code worse.
--
flatten :: Ord n 
        => (TransformUpMX Identity c)
        => c a n -> c a n
flatten = transformUpX' flatten1


-- | Flatten a single nested let-expression.
---
--   TODO: make this work on lazy lets as well.
flatten1
        :: Ord n
        => Exp a n
        -> Exp a n

-- Let ----------------------------------------------------
-- Flatten Nested Lets.
-- @
--      let b1 = (let b2 = def2 in x2) in
--      x1
--
--  ==> let b2 = def2 in 
--      let b1 = x2   in
--      x1
-- @
-- 
flatten1 (XLet a1 (LLet LetStrict b1
            inner@(XLet a2 (LLet LetStrict b2 def2) x2))
               x1)

 | isBName b2
 = flatten1
        $ XLet a1 (LLet LetStrict b1 
               (anonymizeX inner))
               x1

 | otherwise
 = let  x1'       = liftAcrossX [b1] [b2] x1                                 
   in   XLet a2 (LLet LetStrict b2 def2) 
      $ flatten1
      $ XLet a1 (LLet LetStrict b1 x2) 
             x1'


-- Drag 'letregion' out of the top-level of a binding.
-- @
--    let b1 = letregion b2 in x2 in
--    x1
--
-- => letregion b2 in 
--    let b1 = x2 in
--    x1
-- @
--
flatten1 (XLet a1 (LLet LetStrict b1
            inner@(XLet a2 (LLetRegion b2 bs2) x2))
               x1)
 | isBName b2
 = flatten1
        $ XLet a1 (LLet LetStrict b1
                  (anonymizeX inner))
               x1

 | otherwise
 = let  x1'      = liftAcrossX [b1] [b2] x1
   in   XLet a2 (LLetRegion b2 bs2) 
      $ flatten1
      $ XLet a1 (LLet LetStrict b1 x2) 
             x1'


-- Flatten single-alt case expressions.
-- @
--     let b1 = case x1 of 
--                P -> x2 
--     in x3
--
--  => case x1 of 
--       P -> let b1 = x2 
--            in x3
-- @
--
-- * binding must be strict because we force evaluation of x1.
--
flatten1 (XLet  a1 (LLet LetStrict b1 
             inner@(XCase a2 x1 [AAlt p x2]))
                   x3)
 | any isBName $ bindsOfPat p
 = flatten1
        $ XLet  a1 (LLet LetStrict b1
                   (anonymizeX inner))
                   x3

 | otherwise
 = let  x3'     = liftAcrossX [b1] (bindsOfPat p) x3
   in   XCase a2 x1 
           [AAlt p ( flatten1 
                   $ XLet a1 (LLet LetStrict b1 x2)
                             (anonymizeX x3'))]


-- Any let, its bound expression doesn't contain a strict non-recursive
-- let so just flatten the body
flatten1 (XLet a1 llet1 x1)
 = XLet a1 llet1 (flatten1 x1)


-- Case ---------------------------------------------------
-- Flatten all the alternatives in a case-expression.
flatten1 (XCase a x1 alts)
 = XCase a (flatten1 x1) 
           [AAlt p (flatten1 x) | AAlt p x <- alts ]

flatten1 x = x


liftAcrossX :: Ord n => [Bind n] -> [Bind n] -> Exp a n -> Exp a n
liftAcrossX bsDepth bsLevels x
 = let  depth   = length [b | b@(BAnon _) <- bsDepth]
        levels  = length [b | b@(BAnon _) <- bsLevels]
   in   liftAtDepthX levels depth x

