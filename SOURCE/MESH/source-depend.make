############################################################
##   MESH library compilation
#
.SUFFIXES:

# Directory
LDIR := MESH

# Library name
LIBNAME := libt_mesh

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    GEO3D.f90        \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    build_implicit_bdlu.f90   \
    build_implicit_dlu.f90    \
    calc_connface.f90         \
    calc_ustmesh.f90          \
    init_implicit_bdlu.f90    \
    init_implicit_dlu.f90     \
    interpface_gradient_scal.f90 \
    interpface_gradient_vect.f90 \
    interpface_gradn_scal.f90    \
    interpface_gradn_vect.f90    \
    scale_mesh.f90            \
    test_ustmesh.f90          \

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

##GG:>>>
##GG: dependency removed
##$(LIBNAME).target: $($(LDIR).objfiles)
##GG: and replaced
$($(LDIR).libfile): $($(LDIR).objfiles)
##GG:<<<

D_$(LDIR)_SRC := $($(LDIR).objnames:%.o=$(LDIR)/%.f90)


####### Build rules

$(LDIR)_clean: %_clean:
	-rm $($*.libfile) $($*.objfiles) $($*_MOD) $*/depends.make


####### Dependencies

$(LDIR)/depends.make: %/depends.make: $(D_%_SRC)
	$(MAKEDEPENDS) $*

include $(LDIR)/depends.make

