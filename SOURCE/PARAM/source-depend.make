############################################################
##   PARAM library compilation
#
.SUFFIXES:

# Directory
LDIR := PARAM

# Library name
LIBNAME := libt_param

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    MENU_ALE.f90      \
    MENU_AMR.f90      \
    MENU_BOCO.f90     \
    MENU_COUPLING.f90 \
    MENU_GEN.f90      \
    MENU_INIT.f90     \
    MENU_INTEG.f90    \
    MENU_INVERSE.f90  \
    MENU_MPI.f90      \
    MENU_NUM.f90      \
    MENU_SOLVER.f90   \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    def_ale.f90          \
    def_amr.f90          \
    def_boco.f90         \
    def_check.f90        \
    def_connect.f90      \
    def_fctenv.f90       \
    def_init.f90         \
    def_inverse.f90      \
    def_mesh.f90         \
    def_mrf.f90          \
    def_mpi.f90          \
    def_other.f90        \
    def_output.f90       \
    def_param.f90        \
    def_probe.f90        \
    def_project.f90      \
    def_spat.f90         \
    def_time.f90         \
    trait_param.f90      \
    trait_zoneparam.f90  \
    def_coupling.f90     \

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

