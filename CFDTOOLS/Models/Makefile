.SUFFIXES:

default: this.modules this.library
#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(CONFIGDIR)/arch.make
include $(PRJDIR)/cfdproject.make
include $(CONFIGDIR)/rules.make

LIBNAME:=libcfdmodels

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

this.f90modules=   \
  QUANTITY.f90     \

this.f90= $(this.f90modules) \


include $(CONFIGDIR)/libconf.make

include depends.make


