
module DDC.Config.Version
	( ddcName
	, version
	, maintainers )
where
	
-- | The long name is added as a tag line to generated C files,
--	and used in the command line interface.
ddcName	= "The Disciplined Disciple Compiler, version " ++ version
version	= "Alpha 1.2"

-- | Maintainer contact string to put in panic messages.
maintainers	
	=  "    http://trac.haskell.org/ddc\n"
	++ "    or by emailing disciple-cafe@googlegroups.com\n"
