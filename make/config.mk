
# This works for 'Linux', 'Darwin' and 'FreeBSD'.
TARGET_OS := $(shell uname -s | tr 'A-Z' 'a-z')

# This works for 'i386', 'i686' and 'x86_64'.
TARGET_CPU := $(shell uname -m | sed -e "s/^i3/x/" -e "s/^i6/x/")

TARGET         = $(TARGET_OS)-$(TARGET_CPU)
BUILDFLAVOUR   = distro
GHC            = ghc

# This allows you to optionally override TARGET/BUILDFLAVOUR/GHC.
-include make/config-override.deps

# TARGET: Set the target architecture
#    linux-x86
#    linux-x86_64
#    freebsd-x86
#    darwin-x86
#    darwin-x86_64

# BUILDFLAVOUR: Set the optimisations to enable when compiling the compiler
#     distro     - fastest at runtime.
#     devel      - development compile (faster compiler build).
#     devel_prof - development compile with profiling.

