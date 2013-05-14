.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR = ..
CONFIGDIR = $(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME := libcfdmesh

# List of f90 modules
this.f90modules = \
  DEF_USTBOCO.f90  \
  DEFPROBE.f90     \
  PROBECALC.f90    \
  ELEMVTEX.f90     \
  GRID_CONNECT.f90 \
  MESHBASE.f90     \
  MESHCONNECT.f90  \
  MESHGEOM.f90     \
  MESHMRF.f90      \
  MESHPARAMS.f90   \
  TENSOR3.f90      \
  USTMESH.f90      \
  STRMESH.f90      \
  Field/BASEFIELD.f90  \
  Field/GENFIELD.f90   \

# List of f90 files
this.f90 = $(this.f90modules) \
  create_facevtex.f90        \
  seek_bcface_face.f90       \
  seek_bcface_vtex.f90       \

LIBDEPS =
LOCALLINKOPT =

# List of f90 program files
this.f90exe = \

# Dependencies for tools
this.tools:

include $(CONFIGDIR)/libconf.make

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

include depends.make

