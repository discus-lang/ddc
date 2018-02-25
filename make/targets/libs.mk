
# -- Clean the base libraries.
.PHONY : clean-libs
clean-libs :
	@echo "* Cleaning libs"
	@find src/s2 \
		   -name "*.di" \
		-o -name "*.o" \
		-follow \
		| xargs -n 1 rm -f

.PHONY : libs
libs: bin/ddc
	@bin/ddc -build src/s2/base/base.build
