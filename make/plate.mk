
#---- global defs
stumpy		= ../stumpy/bin/stumpy
GHC_FLAGS_PROF	:= -prof -auto-all

# GHC_FLAGS	:= -fglasgow-exts -tmpdir /tmp -O2 -prof -auto-all
GHC_FLAGS	:= -fglasgow-exts -tmpdir /tmp 

# GCC_FLAGS	:= -std=c99 -Werror -Wundef -g -pg -fPIC
GCC_FLAGS	:= -std=c99 -Wundef -fPIC -O3


GHC_INCDIRS 	= $(patsubst %,-i../%/src,$(MODULES))
GHC_INCOBJS	= $(foreach module,$(MODULES),$(shell cat ../$(module)/lib/libH$(module).so.objs))
GHC_INCLIBS	= $(foreach module,$(MODULES),../$(module)/lib/libH$(module).so)

#---- stumpy
src_hss		= $(shell find src -name "*.hss" -follow)

src_stubHS_hs	= $(patsubst %.hss,%.hs,$(src_hss))

src_stubC_c	= $(patsubst %.hss,%.stubC.c,$(src_hss))
src_stubC_h	= $(patsubst %.hss,%.stubC.h,$(src_hss))

.PRECIOUS	: $(src_stubHS_hs)
.PRECIOUS	: $(src_stubC_c)


%.hs %.stubC.c : %.hss
	@echo "* Preprocessing $<"
	$(stumpy) $<
	@echo


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


%.o : stubH = $(wildcard $(patsubst %.hs,%.stubC.h,$<))
%.o : %.hs
	@echo "* Compiling $<"

	$(if $(stubH),								\
		ghc $(GHC_FLAGS) -isrc  $(GHC_INCDIRS) -c $< -#include $(stubH),	\
		ghc $(GHC_FLAGS) -isrc  $(GHC_INCDIRS) -c $<		\
	 )	

	@echo

# %.o-boot
%.hi-boot : %.hs-boot %.o-boot
	@echo "* Compiling $<"
	@ghc $(GHC_FLAGS) -c $< -isrc $(GHC_INCDIRS)
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

# currentDir	=  $(shell pwd)
obj_relName 	=  $(foreach o,$(obj),../$(MODULE)/$(o))






