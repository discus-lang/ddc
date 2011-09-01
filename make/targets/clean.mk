
# -- clean up everything
.PHONY : clean
clean  : cleanWar cleanRuntime cleanLibrary
	@echo "* Cleaning leftovers"
	@find . \
			-name "*.o" \
		-o      -name "*.o-boot" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-o	-name "*.hcr" \
		-o	-name "*.td" \
		-o	-name "*.ti" \
		-o	-name "*.deps" \
		-o      -name "*.deps.inc" \
		-o	-name "*.vo" \
		-o	-name "*.glob" \
		-follow | xargs -n 1 rm -f

	@rm -f doc/haddock/*
	@rm -f src/Config/Config.hs src/Source/Lexer.hs
	@rm -f 	bin/* \
		make/Makefile.deps.bak
