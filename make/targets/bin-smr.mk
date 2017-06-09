# Shimmer executable.

smr-main_src_ds_all = \
	$(shell find src/s2/base         -name "*.ds" -follow) \
	$(shell find src/s2/smr-core/src -name "*.ds" -follow)

# Link smr execurable.
smr-main_obj = $(patsubst %.hs,%.o,$(smr-main_src_ds_all))

# Build smr executable.
bin/smr : $(smr-main_obj) src/s2/ddc-runtime/build/libddc-runtime.a bin/ddc
	@echo "* Building smr"
	@bin/ddc --make src/s2/smr-core/src/Main.ds -o bin/smr

