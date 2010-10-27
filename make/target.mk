# Build target detection and configuration.
include make/config.mk

# The following targets should work:
#     linux-x86
#     linux-x86_64
#     linux-ppc
#     freebsd-x86
#     darwin-x86
#     darwin-x86_64

# Autodetect the build operating system.
#   This works for 'Linux', 'Darwin' and 'FreeBSD'.
TARGET_OS	:= $(shell uname -s | tr 'A-Z' 'a-z')

# Autodetect the build architecture.
#   This works for 'i386', 'i686', 'x86_64' and 'ppc'.
#
#   We don't currently support ppc64 because the only machine available to test
#   this has a 32 bit ghc6. Therefore we automatically convert ppc64 to ppc.
TARGET_ARCH	:= $(shell sh make/goop/getArch.sh)

# The target is the combination of OS and machine architecture.
TARGET		= $(TARGET_OS)-$(TARGET_ARCH)
Target          := $(strip $(TARGET))

# -----------------------------------------------------------------------------
# Target dependent make flags.
# 
#     *ALL* of the target dependent settings should be defined right here.
#   Don't spread target dependent stuff through the rest of the build system.
#

# -- Linux on x86
ifeq "$(Target)" "linux-x86"
GCC_FLAGS           += -fPIC -D BITS=32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- Linux on x86_64
else ifeq "$(Target)" "linux-x86_64"
GCC_FLAGS           += -fPIC -D BITS=64 -m64
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- Linux on ppc
else ifeq "$(Target)" "linux-ppc"
GCC_FLAGS           += -fPIC -D BITS=32 -m32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- FreeBSD on x86
else ifeq "$(Target)" "freebsd-x86"
GCC_FLAGS           += -fPIC -D BITS=32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so 

# -- Darwin on x86
else ifeq "$(Target)" "darwin-x86"
GCC_FLAGS           += -fPIC -D BITS=32 -m32
BUILD_SHARED        := gcc -m32 -dynamiclib -undefined dynamic_lookup 
SHARED_SUFFIX       := dylib

# -- Darwin on x86_64
else ifeq "$(Target)" "darwin-x86_64"
GCC_FLAGS           += -fPIC -D BITS=64 -m64
BUILD_SHARED        := gcc -m64 -dynamiclib -undefined dynamic_lookup
SHARED_SUFFIX       := dylib

else
all : $(error "Unknown Target '$(Target)'. Set this in make/config.mk")

endif
