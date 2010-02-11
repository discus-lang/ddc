
-- Handling of object syntax.
module Source.Rename.Object
	( pushObjectVar
	, popObjectVar
	, peekObjectVar )
where

import Source.Rename.State
import Control.Monad.State
import Shared.Error
import Shared.Var

stage = "Source.Rename.Object"

-- | Push an object variable context.
pushObjectVar :: Var	-> RenameM ()
pushObjectVar	v
 	= modify (\s -> s { stateObjectVar = v : stateObjectVar s })

	
-- | Pop an object variable context.
popObjectVar  :: RenameM Var
popObjectVar	
 = do 	objectVar	<- gets stateObjectVar
	case objectVar of
	 []	-> panic stage "popObjectVar: stack underflow\n"
	 (x:xs)	
	  -> do	modify (\s -> s { stateObjectVar = xs })
 	  	return x

-- | Peek at the current object variable context.
peekObjectVar :: RenameM Var
peekObjectVar
 = do 	objectVar	<- gets stateObjectVar
 	case objectVar of
	 []	-> panic stage "peekObjectVar: stack underflow\n"
	 (x:xs)
	  -> 	return x
