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

