
%.hs : %.x
	@echo "* Preprocessing $<"
	@alex -g $<

%.hi : %.o
	@true


# -- compile Haskell source
%.o : %.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) -isrc $(GHC_INCDIRS) -c $<


# -- compile Haskell boot 
%.hi-boot : %.hs-boot %.o-boot
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) -isrc $(GHC_INCDIRS) -c $< 

%.o : %.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -c $< -o $@ 

%.dep : %.c
	@echo "* Building Deps $<"
	@gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@






