# Default make rules.
include make/build.mk
include make/config/target.mk

# -----------------------------------------------------------------------------
# Runtime system 
packages/ddc-code/sea/%.o : packages/ddc-code/sea/%.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -Ipackages/ddc-code/sea/runtime -c $< -o $@ 


# -- Generic Rules ------------------------------------------------------------
%.hs : %.x
	@echo "* Preprocessing $<"
	@alex -g $<


# Add a dependency from .hi to .o because ghc produces dependencies like
# > A.o : B.hi
# when module A depends on B.
#
# However, this creates a circular dependency because GHC might
# output the .hi before the .o!
# This causes make to rebuild the .hi since the .o is newer.
#
# So in the rule below we touch the .hi file after building the .o.
# This way the .o is older and will not cause a rebuild.
%.hi : %.o
	@true
%.o : %.hs
	touch $(patsubst %.o,%.hi,$@)


%.dep : %.c
	@echo "* Dependencies for $<"
	@gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@


%.o : %.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -c $< -o $@ 


%.o : %.dcl bin/ddc
	@echo "* Compiling $<"
	@bin/ddc -c $<


%.o : %.dcs bin/ddc
	@echo "* Compiling $<"
	@bin/ddc -infer -c $<

