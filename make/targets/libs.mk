
# -- Clean the base libraries.
.PHONY : clean-libs
clean-libs :
	@echo "* Cleaning libs"
	@find src/s1/ddc-code \
		   -name "*.di" \
		-o -name "*.o" \
		-follow \
		| xargs -n 1 rm -f

.PHONY : libs
libs: bin/ddc
	@bin/ddc -build src/s1/ddc-code/tetra/base/base.build
