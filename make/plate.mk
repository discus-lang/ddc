
%.hs : %.x
	@echo "* Preprocessing $<"
	alex -g $<
	@echo

%.hi : %.o
	@true


# -- compile Haskell source
%.o : %.hs
	@echo "* Compiling $<"
	$(GHC) $(GHC_FLAGS) -isrc $(GHC_INCDIRS) -c $<
	@echo


# -- compile Haskell boot 
%.hi-boot : %.hs-boot %.o-boot
	@echo "* Compiling $<"
	$(GHC) $(GHC_FLAGS) -isrc $(GHC_INCDIRS) -c $< 
	@echo

%.o : %.c
	@echo "* Compiling $<"
	gcc $(GCC_FLAGS) -c $< -o $@ 
	@echo

%.dep : %.c
	@echo "* Building Deps $<"
	gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@
	@echo






