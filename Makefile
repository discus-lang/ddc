
# targets:
# 	all
#	clean
#	war		cleanWar
#	runtime 	cleanRuntime
#	


all	: bin/ddc bin/war

include make/build.mk
include make/plate.mk
-include make/Makefile.deps
-include runtime/*.dep

# ----- trauma
bin/ddc	: $(obj) $(GHC_INCOBJS)
	@echo "* Linking $@"
	$(GHC) -o bin/ddc $^ $(LIBS) -package containers -package mtl
# -prof
	

# ----- war
bin/war : test/War.hs
	$(GHC) -fglasgow-exts -isrc --make test/War.hs -o bin/war

.PHONY  : cleanWar
cleanWar :
	@echo "* Cleaning up war"
	@find test \
			-name "*.dump-*.*"  \
		-o	-name "*.graph-*.dot" \
		-o	-name "*.di"    \
		-o	-name "*.gdl"   \
		-o	-name "*.o"     \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.bin"   \
		-o	-name "*.out"   \
		-o 	-name "*.diff"  \
		-o	-name "*.tix"   \
		-follow | xargs -n 1 rm -f

	@find library \
			-name "*.dump-*.*"  \
			-name "*.graph-*.dot" \
		-o	-name "*.di" 	\
		-o	-name "*.gdl"	\
		-o	-name "*.o"	\
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.bin" 	\
		-o	-name "*.out"	\
		-o 	-name "*.diff"  \
		-o	-name "*.tix"	\
		-follow | xargs -n 1 rm -f
	
	@echo

	
# ----- runtime	
runtime_c = \
	$(shell ls runtime/*.c) \
	$(shell find runtime/Prim -name "*.c")

runtime_dep	= $(patsubst %.c,%.dep,$(runtime_c))
runtime_o	= $(patsubst %.c,%.o,$(runtime_c))

runtime/ddc-runtime.so : $(runtime_o)
	@echo "* Linking $@"
	gcc -shared -o $@ $^
	@echo

runtime/ddc-runtime.a  : $(runtime_o)
	@echo "* Building $@"
	ar r $@ $^
	@echo

.PHONY  : runtime
runtime : runtime/ddc-runtime.so runtime/ddc-runtime.a

.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning up runtime"
	@find runtime \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o	-name "*.a" \
		-follow | xargs -n 1 rm -f
	@echo		


#---- clean
clean  : cleanWar
	@echo "* Cleaning up"
	@find . \
			-name "*.o" \
		-o	-name "*.so" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-o	-name "*.hcr" \
		-o	-name "*.td" \
		-o	-name "*.ti" \
		-o	-name "Makefile.deps" \
		-follow | xargs -n 1 rm -f
		
	@rm -f 	bin/* \
		make/Makefile.deps.bak 

	@echo

.PHONY : clean


#---- deps
.PHONY : deps
deps : make/Makefile.deps $(runtime_dep)
	@echo

make/Makefile.deps : $(src_hs)
	@echo "* Building dependencies"
	@$(GHC) -isrc -M $^ -optdep-f -optdepmake/Makefile.deps $(GHC_INCDIRS)
	@rm -f make/Makefile.deps.bak
	@echo


# -- build documents
nodoc	= \
	src/Source/Lexer.hs \
	src/Util/Tunnel.hs \
	src/Source/Type/SlurpA.hs \
	src/Source/Type/SlurpX.hs
	


.PHONY	: doc
doc	: $(filter-out $(nodoc),$(src_hs))
	@echo "* Building documentation"
	@haddock -h -o doc/haddock --ignore-all-exports $^ 
