############################################################
##   Mesh library compilation
#
.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR := ..

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

# Directory
LDIR := Mesh

# Library name
LIBNAME := libcfdmesh

####### Files

# List of f90 modules
$(LDIR).f90MODFILES := \
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
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    create_facevtex.f90        \
    seek_bcface_face.f90       \
    seek_bcface_vtex.f90       \

# List of needed libraries
LIBDEPS :=
LOCALLINKOPT :=

# List of f90 program files
$(LDIR).f90exefiles := \

# Dependencies for tools
this.tools: $($(LDIR).f90exefiles:%.f90=$(PRJEXEDIR)/%)

include $(CONFIGDIR)/libconf.make

##GG:>>>
##GG: dependency modified from:
##this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: to:
this.library: $(PRJOBJDIR) $(PRJLIBDIR) $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)
##GG: or:
##default: $(PRJOBJDIR) $(PRJLIBDIR)
##GG:<<<

include depends.make

