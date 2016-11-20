module DDC.Core.Salt.Transform.Slotify
        (slotifyModule)
where
import DDC.Core.Salt.Transform.Slotify.Inject
import DDC.Core.Salt.Transform.Slotify.Object
import DDC.Core.Salt.Transform.Slotify.Replace
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Exp.Annot
import DDC.Core.Module
import DDC.Data.Pretty
import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Compounds                as A    
import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Core.Check                         as Check
import qualified DDC.Core.Simplifier                    as Simp
import qualified DDC.Core.Simplifier.Recipe             as Simp
import qualified DDC.Core.Transform.Namify              as Namify
import qualified DDC.Core.Transform.Reannotate          as Reannotate
import qualified DDC.Type.Env                           as Env
import qualified Control.Monad.State.Strict             as State
import qualified Data.Map                               as Map


-------------------------------------------------------------------------------
-- | Insert slot allocations for heap objects.
slotifyModule
        :: (Show a, Pretty a)
        => a
        -> Module (AnTEC a A.Name) A.Name
        -> Either (A.Error  (AnTEC a A.Name))
                  (Module (AnTEC a A.Name) A.Name)

slotifyModule a mm@ModuleCore{}
 | mmStrip               <- Reannotate.reannotate annotTail mm
 , XLet aa (LRec bxs) x1 <- moduleBody mmStrip
 = let
        bxs'    = map (slotifyLet a) bxs
        mmSlots = mmStrip { moduleBody = XLet aa (LRec bxs') x1 }

        anorm   = Simp.anormalize (Namify.makeNamifier A.freshT)
                                          (Namify.makeNamifier A.freshX)

        mmANF   = Simp.result $ fst
                $ flip State.runState 0
                $ Simp.applySimplifier
                        A.profile Env.empty Env.empty anorm mmSlots

   in    case Check.checkModule 
                (Check.configOfProfile A.profile) 
                mmANF Check.Recon of

           -- Couldn't reconstruct type annotations.
           (Left err, _checkTrace)
             -> error ("slotifyModule:\n" ++ renderIndent (ppr err)) 
                   -- TODO how to report error properly

           (Right mmCheck, _checkTrace)
             -> Right mmCheck

 | otherwise
 = Left (A.ErrorNoTopLevelLetrec mm)


-- Top level let bindings ------------------------------------------------------
slotifyLet
        :: Show a
        => a
        -> (Bind A.Name, Exp a A.Name)
        -> (Bind A.Name, Exp a A.Name)

slotifyLet a (BName n t, x)
 = (BName n t, slotifySuper a x)

slotifyLet _a bx
 = bx


-- Super -----------------------------------------------------------------------
slotifySuper
        :: Show a
        => a
        -> Exp a A.Name
        -> Exp a A.Name

slotifySuper a xx
 = let  
        -- Split super parameters frm the body expression.
        (bsParam, xBody)  
                = case takeXLamFlags xx of
                        Nothing         -> ([], xx)
                        Just (bs, x)    -> (bs, x)

        ntsObj          = objectsOfExp xx
        ntsObj'         = Map.toList ntsObj

        nSlot n         = A.NameExt n "slot"
        xSlot n         = XVar a (UName (nSlot n))
        tSlot t         = A.tPtr A.rTop t
        bSlot n t       = BName (nSlot n) (tSlot t)

        xPeekSlot  n t  = A.xPeek a A.rTop t (xSlot n) 
        xPokeSlot  n t  = A.xPoke a A.rTop t (xSlot n) (XVar a (UName n))

        wrapPokeSlot n t x
                = XLet a (LLet (BNone A.tVoid) (xPokeSlot n t)) x

        -- Get level-0 binders
        args    = Map.fromList [ (n, ()) | (False, BName n _) <- bsParam ]

        -- Function to wrap an expression in let-bindings that create stack
        -- slots for each of the arguments passed to the super.
        allocSlotsArg x
                = foldr ($) x
                $ [ XLet a (LLet (bSlot n t) 
                                 (A.xAllocSlotVal a tR (XVar a (UName n))))
                  | (n, t)       <- Map.toList $ ntsObj `Map.intersection` args
                  , Just (tR, _) <- [A.takeTPtr t] ]

        -- Function to wrap an expression in let-bindings that create stack
        -- slots for each of the boxed values bound in the body of the super.
        allocSlotsBody x    
                = foldr ($) x
                $ [ XLet a (LLet (bSlot n t) (A.xAllocSlot a tR))
                  | (n, t)       <- Map.toList $ ntsObj `Map.difference`   args
                  , Just (tR, _) <- [A.takeTPtr t] ]

        -- Function to find bound occurrences of a variable that is being
        -- represented by a stack slot and peek the current from the slot at
        -- every occurrence.
        substPeeks x    
                = flip replaceX x 
                $ Map.fromList
                $ [ (n, xPeekSlot n t)
                  | (n, t)      <- ntsObj' ]

        -- Function to find the binding occurrences of variables that are being 
        -- represented by stack slots and poke the value into the slot as soon
        -- as it is defined.
        injectPokesL x  
                = flip injectX x 
                $ Map.fromList
                  [ (n, wrapPokeSlot n t)
                  | (n, t)      <- ntsObj' ]

   in   makeXLamFlags a bsParam
                $ allocSlotsArg
                $ allocSlotsBody
                $ injectPokesL
                $ substPeeks xBody

