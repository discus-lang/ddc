
module Module.Main
	(makeMainC)
where

---
-- makeMainC
--	Makes C source for the Main module which defined the top level main() function.
--	The main() function initialises all the modules then calls traumaMain().
--
makeMainC ::	[String] 	-> String
makeMainC	moduleNames
 
	-- include the RTS so we get the Obj definition
 	= "#include \"Trauma/RTS.h\"\n"
	++ "\n"

	-- import the traumaMain symbol
	++ "extern Obj* traumaMain (void);\n"

	-- import the init functions for each module.
	++ (concat $ map (\n -> "extern void _traumaInit_" ++ n ++ " (void);\n") moduleNames)
	++ "\n"

	-- the main entry point.
	++ "int main (int argc, char** argv)\n"
	++ "{\n"

	-- call the module init functions.
	++ (concat $ map (\s -> "        _traumaInit_" ++ s ++ " ();\n") moduleNames)
	++ "\n"

	-- call traumaMain
	++ "        traumaMain ();\n"
	++ "\n"
	++ "}\n\n"
	

