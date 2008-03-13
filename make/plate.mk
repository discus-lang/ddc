
#---- global defs
GHC_INCDIRS  = $(patsubst %,-i../%/src,$(MODULES))
GHC_INCOBJS  = $(foreach module,$(MODULES),$(shell cat ../$(module)/lib/libH$(module).so.objs))
GHC_INCLIBS  = $(foreach module,$(MODULES),../$(module)/lib/libH$(module).so)

#---- alex
src_alex_x	=  $(shell find src -name "*.x" -follow)
src_alex_hs	=  $(patsubst %.x,%.hs,$(src_alex_x))

%.hs : %.x
	@echo "* Preprocessing $<"
	alex -g $<
	@echo


#---- happy
src_happy_y	=  $(shell find src -name "*.y" -follow)
src_happy_hs	=  $(patsubst %.y,%.hs,$(src_happy_y))


%.hs : %.y
	@echo "* Preprocessing $<"
	happy $< -i$<.info
	@echo


#---- ghc
src_hs_there	=  $(shell find src -name "*.hs" -follow)
src_hs		+= $(src_hs_there)
src_hs		+= $(src_stubHS_hs)
src_hs		+= $(src_alex_hs)
src_hs		+= $(src_happy_hs)


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
	@$(GHC) $(GHC_FLAGS) -isrc $(GHC_INCDIRS) -c $< 
	@echo


#---- gcc
src_c_there	=  $(shell find src -name "*.c" -follow)
src_c		+= $(src_c_there)
src_c		+= $(src_stubC_c)

%.o : %.c
	@echo "* Compiling $<"
	gcc $(GCC_FLAGS) -c $< -o $@ 
	@echo

%.dep : %.c
	@echo "* Building Deps $<"
	gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@
	@echo


#---- objs
obj		=  $(patsubst %.hs,%.o,$(src_hs_there))
obj		+= $(patsubst %.c,%.o,$(src_c))

obj_relName 	=  $(foreach o,$(obj),../$(MODULE)/$(o))






