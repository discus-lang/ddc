GHC=/home/benl/devel/ghc/ghc-HEAD-work/compiler/stage2/ghc-inplace

# GHC_FLAGS	:= -fglasgow-exts -tmpdir /tmp -O2 -prof -auto-all
GHC_FLAGS	:= -fhpc -fallow-undecidable-instances -fglasgow-exts -tmpdir /tmp 

# GCC_FLAGS	:= -std=c99 -Werror -Wundef -g -pg -fPIC
GCC_FLAGS	:= -std=c99 -Wundef -fPIC -O3
