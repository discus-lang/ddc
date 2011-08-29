
# Find Coq proof scripts
src_coq_v 	= $(shell find proof -name "*.v" -follow)

# Coqc makes a .vo and a .glob from each .v file.
src_coq_vo	= $(patsubst %.v,%.vo,$(src_coq_v))


proof/DDC/%.vo proof/DDC/%.glob : proof/DDC/%.v
	@echo "* Checking $<"
	@$(COQC) -R proof/DDC DDC  $<

# Build dependencies for Coq proof scripts.
.PHONY: proofdeps
proofdeps: make/deps/proof.deps
	
make/deps/proof.deps : $(src_coq_v)
	@echo "* Building proof dependencies"
	@$(COQDEP) -R proof/DDC DDC $(src_coq_v) > make/deps/proof.deps
	@cp make/deps/proof.deps make/deps/proof.deps.inc
	@echo

	
	
