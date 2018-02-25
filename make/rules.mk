# Default make rules.
include make/build.mk
include make/config/target.mk

%.hs : %.x
	@echo "* Preprocessing $<"
	@alex -g $<


# This ':' is equivalent to 'true', but much faster. Strange.
%.hi : %.o
	@:


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

