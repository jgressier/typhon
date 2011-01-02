.SUFFIXES:

default: this.library this.modules 
#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(CONFIGDIR)/arch.make
include $(PRJDIR)/cfdproject.make
include $(CONFIGDIR)/rules.make

LIBNAME:=libcfdmesh

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

this.f90modules=   \
  DEF_USTBOCO.f90  \
  ELEMVTEX.f90     \
  GRID_CONNECT.f90 \
  MESHPREC.f90     \
  MESHBASE.f90     \
  MESHPARAMS.f90   \
  TENSOR3.f90      \
  USTMESH.f90      \
  VEC2D.f90        \
  VEC3D.f90        \
  Field/BASEFIELD.f90  \
  Field/GENFIELD.f90   \

this.f90= $(this.f90modules) \
  create_face_connect.f90    \
  create_facevtex.f90        \
  create_ustboco.f90         \
  reorder_ustconnect.f90     \
  seek_bcface_face.f90       \
  seek_bcface_vtex.f90       \

this.tools:


include $(CONFIGDIR)/libconf.make

include depends.make


