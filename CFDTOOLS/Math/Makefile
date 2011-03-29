.SUFFIXES:

default: this.library this.modules
#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME:=libcfdmath

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

this.f90modules= \
  VEC2D.f90         \
  VEC3D.f90         \
  FCT_CONTAINER.f90 \
  FCT_DEF.f90       \
  FCT_ENV.f90       \
  FCT_EVAL.f90      \
  FCT_FUNC.f90      \
  FCT_MATH.f90      \
  FCT_NODE.f90      \
  FCT_PARSER.f90    \

this.f90= $(this.f90modules) \

this.tools:


include $(CONFIGDIR)/libconf.make

include depends.make


