
# -- Autodetect the build target
# This works for 'Linux', 'Darwin' and 'FreeBSD'.
TARGET_OS := $(shell uname -s | tr 'A-Z' 'a-z')

# This works for 'i386', 'i686', 'x86_64' and 'ppc'.
# Currently don't support ppc64 because the only machine available to test
# this has a 32 bit ghc6. Therefore we automatically convert ppc64 to ppc.
TARGET_CPU := $(shell uname -m | sed -e "s/^i3/x/" -e "s/^i6/x/" -e "s/^ppc64/ppc/")

TARGET		= $(TARGET_OS)-$(TARGET_CPU)
BUILDFLAVOUR	= distro
GHC		= ghc
THREADS		= 2

# This allows you to optionally override the above config
-include make/config-override.deps

# TARGET: The following platforms should work
#    linux-x86
#    linux-x86_64
#    linux-ppc
#    freebsd-x86
#    darwin-x86
#    darwin-x86_64

# BUILDFLAVOUR: Set the optimisations to enable when compiling the compiler
#     distro     - fastest at runtime.
#     devel      - development compile (faster compiler build).
#     devel_prof - development compile with profiling.

