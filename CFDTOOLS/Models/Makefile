.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR = ..
CONFIGDIR = $(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME := libcfdmodels

# List of f90 modules
this.f90modules = \
  QUANTITY.f90     \

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

