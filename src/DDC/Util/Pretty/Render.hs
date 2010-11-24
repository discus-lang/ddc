{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

-- | Rendering of PrettyPrims to a string.
module DDC.Util.Pretty.Render
	( renderWithMode
	, nextTabStopCol)
where
import DDC.Util.Pretty.Base
import Data.Monoid
import Data.Text.Lazy.Builder		(Builder)
import qualified Data.Text.Lazy		as T
import qualified Data.Text.Lazy.Builder	as B

-- | Render a pretty string in a given mode.
renderWithMode :: mode -> StrMode mode -> String
renderWithMode mode str
	= T.unpack $ B.toLazyText $ snd $ render mode 0 0 str

(<>)		:: Monoid a => a -> a -> a
(<>)		= mappend

fromString	= B.fromLazyText . T.pack

-- | Render a pretty string.
render 	:: mode -> Int -> Int -> StrMode mode -> (Int, Builder)
render mode colIndent col ss
 = let	down	= render mode colIndent
   in case ss of
	PBlank
	 -> (col, mempty)

	PString	str
	 -> let	text	= T.pack str
	    in	( col + (fromIntegral $ T.length text)
		, B.fromLazyText text)
	
	PChar c	
	 -> (col + 1, B.singleton c)

	PNewLine
	 -> let	pad	= fromString $ replicate colIndent ' '
	    in	(colIndent, B.singleton '\n' <> pad)

	PModal f
	 -> down col (f mode)

	PAppend []
	 -> (col, mempty)

	PAppend (s:strs)
	 -> let (col1, str1)	 = down col s
		(col2, strsRest) = down col1 (PAppend strs)
	    in	(col2, str1 <> strsRest)

	PShift str
	 -> let	colNext	= nextTabStopCol col
		pad	= replicate (colNext - col) ' '
	    in	down col (PAppend [PString pad, str])

	PIndent str
	 -> let	colNext	= nextTabStopCol (colIndent + 1)
		pad	= replicate (colNext - col) ' '
	    in	render mode colNext col
			(PAppend [PString pad, str])

	PSetColumn colNext str
	 -> let	pad	= replicate (colNext - col) ' '
 	    in	render mode colNext colNext
			(PAppend [PString pad, str])
	
	PPadLeft _n _c str
	 -> down col str
	
	PPadRight _n _c str
	 -> down col str


-- | If we're past the previous tabstop, then advance to the next one.
nextTabStopCol :: Int -> Int
nextTabStopCol col
 = let	stop	= col `div` 8
	slack	= col `mod` 8
   in	if slack == 0
		then stop * 8
		else (stop + 1) * 8
