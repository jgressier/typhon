.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR = ..
CONFIGDIR = $(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME := libcfdbase

# List of f90 modules
this.f90modules = \
  COLOR.f90        \
  CONNECTIVITY.f90 \
  CONNECT_CSR.f90  \
  ENDIAN.f90       \
  FTNARGS.f90      \
  IO_UNIT.f90      \
  IOCFD.f90        \
  LIBSORT.f90      \
  MESHPREC.f90     \
  PACKET.f90       \
  STRING.f90       \
  TIMER.f90        \

# List of f90 files
this.f90 = $(this.f90modules) \

LIBDEPS =
LOCALLINKOPT =

# List of f90 program files
this.f90exe = \

# Dependencies for tools
this.tools:

include $(CONFIGDIR)/libconf.make

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

include depends.make

