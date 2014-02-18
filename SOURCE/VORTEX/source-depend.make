############################################################
##   VORTEX library compilation
#
.SUFFIXES:

# Directory
LDIR := VORTEX

# Library name
LIBNAME := libt_vortex

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    PAN2D_LIN.f90    \
    MENU_VORTEX.f90  \
    VORTEX2D.f90     \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    calc_induced_velocities.f90 \
    def_boco_vortex.f90         \
    def_init_vortex.f90         \
    def_model_vortex.f90        \
    fillmat_sing_effects.f90    \
    get_singularity_nodes.f90   \
    init_boco_vort.f90          \
    init_vort_ust.f90           \

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

