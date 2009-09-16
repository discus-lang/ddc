
-- | Rendering of PrettyPrims to a string.
module DDC.Util.Pretty.Render 
	(render)
where

import DDC.Util.Pretty.Base
import Data.List

-- | Render state.
data RenderS
	= RenderS
	{ stateTabWidth		:: Int		-- ^ current tab width
	, stateIndent		:: Int		-- ^ current start of line indent
	, stateCol		:: Int }	-- ^ current position of cursor


-- | Initial render state.
initRenderS
	= RenderS
	{ stateTabWidth		= 8
	, stateIndent		= 0 
	, stateCol		= 0 }
	

-- | Render a pretty prim as a string.
render	:: PrettyPrim -> String
render xx	
	= render' initRenderS xx

render'	state xx
 	= spaceTab state $ reduce state xx


-- | Reduce a PrettyPrim to some simpler ones in preparation for rendering.
reduce 	:: RenderS -> PrettyPrim -> [PrettyPrim]
reduce	state xx
 = case xx of
	PNil			-> [PNil]
 	PString	s		-> map PChar s
	PChar c			-> [PChar c]
	PList   zz@(PChar c:xs)	-> zz

	PList 	zz	
	 -> [PChar '[']
	 ++ concat (intersperse (map PChar ", ") $ map (reduce state) zz) 
	 ++ [PChar ']']

	PAppend ss		-> concatMap (reduce state) ss
	
	PIndent ss	
	 ->   [ PTabAdd (stateTabWidth state)]
	   ++ reduce state ss
	   ++ [PTabAdd (- (stateTabWidth state))]
	 
	PTabNext		-> [PTabNext]
	PPadLeft  n c p		-> [PPadLeft n c  (PAppend $ reduce state p)]
	PPadRight n c p		-> [PPadRight n c (PAppend $ reduce state p)]
	

-- | Render a pretty things as characters.
spaceTab :: RenderS -> [PrettyPrim] -> String
spaceTab state xx
 = case xx of
	[]		-> []

	PNil : xs	
	 ->    spaceTab state xs

 	PTabAdd i  :xs	
	 -> let state'	= state
	 		{ stateIndent	= stateIndent state + i }
	    in spaceTab state' xs
				

	PTabNext : xs
	 -> let tabHere	= stateCol state `div` stateTabWidth state
	 	tabNext	= tabHere + 1 * stateTabWidth state
		pad	= replicate (tabNext - stateCol state) ' '

	 	state'	= state
	 		{ stateCol	= tabNext }

	   in	pad ++ spaceTab state' xs

	PChar '\n' : PTabAdd i : xs
	 -> spaceTab state (PTabAdd i : PChar '\n' : xs)

	PChar '\n' : xs	
	 -> let	pad	= replicate (stateIndent state) ' '
	 	state'	= state
	 		{ stateCol	= stateIndent state }

	    in  '\n' : (pad ++ spaceTab state' xs)
		
	PChar x : xs
	 -> let state'	= state
	 		{ stateCol	= stateCol state + 1 }
	    in  x : spaceTab state' xs

	PAppend xx : xs
	 -> spaceTab state (xx ++ xs)

	PPadLeft n c p : xs
	 -> let cs	= spaceTab state [p]
		csLen	= length cs
		colLen	= max csLen n
		state'	= state { stateCol = stateCol state + colLen }
	    in	cs ++ replicate (n - length cs) c ++ spaceTab state' xs
	    
	PPadRight n c p : xs
	 -> let cs	= spaceTab state [p]
		csLen	= length cs
		colLen	= max csLen n
		state'	= state { stateCol = stateCol state + colLen }
	    in	replicate (n - length cs) c ++ cs ++ spaceTab state' xs
