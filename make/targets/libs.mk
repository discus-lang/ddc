
# -- Clean the base libraries.
.PHONY : clean-libs
clean-libs :
	@echo "* Cleaning libs"
	@find packages/ddc-code \
		   -name "*.di" \
		-o -name "*.o" \
		-follow \
		| xargs -n 1 rm -f
