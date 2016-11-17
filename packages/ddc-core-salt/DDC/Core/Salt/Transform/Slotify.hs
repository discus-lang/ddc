{-# OPTIONS_GHC -w #-}
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
import Data.Map                                         (Map)
import Data.Set                                         (Set)
import qualified DDC.Core.Salt                          as A
import qualified DDC.Core.Salt.Compounds                as A    
import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Core.Check                         as Check
import qualified DDC.Core.Simplifier                    as Simp
import qualified DDC.Core.Simplifier.Recipe             as Simp
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Namify              as Namify
import qualified DDC.Core.Transform.SubstituteXX        as Subst
import qualified DDC.Core.Transform.Reannotate          as Reannotate
import qualified DDC.Type.Env                           as Env
import qualified Control.Monad.State.Strict             as State
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set

import Data.List

import Debug.Trace

---------------------------------------------------------------------------------------------------
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

slotifyLet a bx
 = bx


-- Super -----------------------------------------------------------------------
slotifySuper
        :: Show a
        => a
        -> Exp a A.Name
        -> Exp a A.Name

slotifySuper a xx
 = let  objs            = objectsOfExp xx
        objs'           = Map.toList objs

        nSlot n         = A.NameExt n "slot"
        xSlot n         = XVar a (UName (nSlot n))
        tSlot t         = A.tPtr A.rTop t
        bSlot n t       = BName (nSlot n) (tSlot t)

        xPeekSlot  n t  = A.xPeek a A.rTop t (xSlot n) (A.xNat a 0)
        xPokeSlot  n t  = A.xPoke a A.rTop t (xSlot n) (A.xNat a 0) (XVar a (UName n))
        xPokeSlot' n t  = XLet a (LLet (BNone A.tVoid) (xPokeSlot n t))

        allocs          = [ XLet a (LLet (bSlot n t) (A.xAllocSlot a tR))
                          | (n, t)              <- objs'
                          , Just (tR, _)        <- [A.takeTPtr t] ]

        allocSlots x    = foldr ($) x allocs

        peeks           = Map.fromList
                        $ [ (n, xPeekSlot n t)
                          | (n, t)              <- objs' ]


        substPeeks x    = replaceX peeks x


        pokes           = Map.fromList
                          [ (n, xPokeSlot' n t)
                          | (n, t)              <- objs' ]

        injectPokesL x  = injectX pokes x

   in  case takeXLamFlags xx of
         Nothing
          -> allocSlots $ injectPokesL $ substPeeks xx

         Just (bs, xx')
          -> let
                -- Get level-0 binders
                args            = Map.fromList [ (n, ()) | (False, BName n _) <- bs ]

                pokes           = [ xPokeSlot' n t
                                  | (n, t)      <- Map.toList (objs `Map.intersection` args) ]

                injectPokesA x  = foldr ($) x pokes

             in makeXLamFlags a bs $ allocSlots
                                   $ injectPokesA
                                   $ injectPokesL
                                   $ substPeeks xx'

