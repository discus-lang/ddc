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
TARGET_OS \
	:= $(shell uname -s \
		| tr 'A-Z' 'a-z' \
		| sed 's/cygwin_nt-.*/cygwin/' \
		| sed 's/mingw32_nt-.*/mingw/' )

# Autodetect the build architecture.
#   This works for 'i386', 'i686', 'x86_64' and 'ppc'.
#
#   We don't currently support ppc64 because the only machine available to test
#   this has a 32 bit ghc6. Therefore we automatically convert ppc64 to ppc.
TARGET_ARCH	:= $(shell sh make/config/goop/getArch.sh)

# The target is the combination of OS and machine architecture.
TARGET		= $(TARGET_OS)-$(TARGET_ARCH)
Target          := $(strip $(TARGET))

# -----------------------------------------------------------------------------
# Target dependent settings.
# 
#  *ALL* of the target dependent settings should be defined right here.
#  Don't spread target dependent stuff through the rest of the build system.
#
#  The settings are:
#
#  GCC_FLAGS		-- Flags to use when compiling .c files.
#  GCC_LINK_SHARED	-- Command to use to link a shared library.
#
#  SHARED_SUFFIX	-- File name suffix for shared libraries (eg .so or .dynlib)
#			-- If the platform doesn't support shared libs then don't define it.
#

# -- Linux on x86
ifeq "$(Target)" "linux-x86"
GCC_FLAGS		+= -fPIC -D BITS=32
GCC_LINK_SHARED		:= gcc -shared
SHARED_SUFFIX		:= so
BITS                    := 32

# -- Linux on x86_64
else ifeq "$(Target)" "linux-x86_64"
GCC_FLAGS		+= -fPIC -D BITS=64 -m64
GCC_LINK_SHARED		:= gcc -shared
SHARED_SUFFIX		:= so
BITS                    := 64

# -- Linux on ppc
else ifeq "$(Target)" "linux-ppc"
GCC_FLAGS		+= -fPIC -D BITS=32 -m32
GCC_LINK_SHARED		:= gcc -shared
SHARED_SUFFIX		:= so
BITS                    := 32

# -- FreeBSD on x86
else ifeq "$(Target)" "freebsd-x86"
GCC_FLAGS		+= -fPIC -D BITS=32
GCC_LINK_SHARED		:= gcc -shared
SHARED_SUFFIX		:= so 
BITS                    := 32

# -- Darwin on x86
else ifeq "$(Target)" "darwin-x86"
GCC_FLAGS		+= -fPIC -D BITS=32 -m32
GCC_LINK_SHARED		:= gcc -m32 -dynamiclib -undefined dynamic_lookup 
SHARED_SUFFIX		:= dylib
BITS                    := 32

# -- Darwin on x86_64
else ifeq "$(Target)" "darwin-x86_64"
GCC_FLAGS		+= -fPIC -D BITS=64 -m64
GCC_LINK_SHARED		:= gcc -m64 -dynamiclib -undefined dynamic_lookup 
SHARED_SUFFIX		:= dylib
BITS                    := 64

# -- Windows{XP,7}/Cygwin on x86
else ifeq "$(Target)" "cygwin-x86"
GCC_FLAGS		+= -D BITS=32 -m32
GCC_LINK_SHARED		:= gcc -shared
BITS                    := 32

# -- Windows/MinGW on x86
else ifeq "$(Target)" "mingw-x86"
GCC_FLAGS           += -D BITS=32 -m32
GCC_LINK_SHARED     := gcc -shared
BITS                := 32

else
all : $(error "Unknown Target '$(Target)'. Set this in make/config.mk")

endif
