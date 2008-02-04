
module Main.Version
	( ddcName
	, version 
	, maintainers)

where

-- | The long name is added as a tag line to generated C files,
--	and used in the command line interface
ddcName		= "The Disciplined Disciple Compiler, version " ++ version
version		= "Alpha1"

-- | Maintainer contact string to put in panic messages
maintainers	
	=  "    http://code.google.com/p/disciple/issues\n"
	++ "    or by emailing ddc-bugs@majestic.fastmail.fm\n"
