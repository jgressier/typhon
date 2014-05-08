############################################################
##   MGRID library compilation
#
.SUFFIXES:

# Directory
LDIR := MGRID

# Library name
LIBNAME := libt_mgrid

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    DEFFIELD.f90     \
    LIMITER.f90      \
    MATFREE.f90      \
    MESHALE.f90      \
    MGRID.f90        \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    build_implicit_bdlu.f90   \
    build_implicit_dlu.f90    \
    init_implicit_bdlu.f90    \
    init_implicit_dlu.f90     \
    calcboco_connect.f90           \
    calcboco_connect_match.f90     \
    calcboco_connect_per_match.f90 \
    calcboco_gen.f90               \
    calcboco_ust.f90               \
    calcboco_ust_extrapol.f90      \
    calcboco_ust_sym.f90           \
    calc_gradient.f90            \
    calc_gradient_gauss.f90      \
    calc_gradface_svm.f90        \
    convert_to_svm.f90           \
    convert_to_svm_cub.f90       \
    convert_to_svm_4wang.f90     \
    convert_to_svm_4kris.f90     \
    raffin_iso_tri.f90           \
    distrib_field.f90            \
    extractpart_grid.f90         \
    grid_preproc.f90             \
    grid_ustpreproc.f90          \
    gmres_free.f90               \
    integ_ustboco.f90            \
    postlimit_monotonic.f90      \
    postlimit_barth.f90          \
    precalc_grad_lsq.f90         \
    splitquadto2x2.f90           \
    splitquadto3x3.f90           \
    update_ustboco_ghostcell.f90 \
    update_ustboco_ghostface.f90 \

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

