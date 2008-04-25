
module Module.Scrape
	( Scrape (..))
where

import Shared.Var	(Var, Module)
import Source.Lexer

data Scrape
	= Scrape
	{ scrapeName	:: Module
	, scrapeImports	:: [Module] }

