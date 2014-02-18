############################################################
##   TYPHON code compilation - MAIN
#
.SUFFIXES:

# Directory
LDIR := MAIN

# Library name
LIBNAME := libt_main

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    MODWORLD.f90     \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    accumulfluxcorr.f90        \
    analyse.f90                \
    analyse_zone.f90           \
    build_implicit.f90         \
    calc_cpl_flux.f90          \
    calc_cpl_temp.f90          \
    calc_hres_states.f90       \
    calc_rhs.f90               \
    calcdifflux.f90            \
    calcul_raccord.f90         \
    check_end_cycle.f90        \
    choixcorrection.f90        \
    correction.f90             \
    corr_varprim.f90           \
    echange.f90                \
    echange_zonedata.f90       \
    echange_zonematch.f90      \
    flux_to_res.f90            \
    implicit_solve.f90         \
    init_boco.f90              \
    init_bocohisto.f90         \
    init_coupling.f90          \
    init_implicit.f90          \
    init_inverse.f90           \
    init_probes.f90            \
    init_world.f90             \
    init_zone.f90              \
    integration.f90            \
    integration_cycle.f90      \
    integration_cycle_inverse.f90 \
    integration_cyclezone.f90  \
    integration_grid.f90       \
    integzone_tstep_lagrange.f90 \
    integzone_tstep_gridtree.f90  \
    integ_treelevel.f90        \
    inverse_calc_sensi.f90     \
    mpi_strategy_pre.f90       \
    mpi_strategy_post.f90      \
    output_result.f90          \
    readallmesh.f90            \
    treelevel_explicit.f90     \
    treelevel_rungekutta.f90   \
    tstep_implicit.f90         \
    update_couplingboco.f90    \
    write_bocohisto.f90        \
    write_monitors_cycle.f90   \
    write_monitors_iteration.f90 \

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

# SVNREV : version number
SVNREV := $(shell svnversion 2> /dev/null || echo unknown)

# SVNREVSTR : fortran code line defining the string for the version number
SVNREVSTR := character(len=20), parameter :: svnrev = '$(SVNREV)'

# SVNREVFILE : name of the file which sould contain SVNREVSTR
SVNREVFILE := ../include/svnrev.h

# SVNREVDEP : defined to SVNREVFORCE if SVNREVFILE does not contain SVNREVSTR
SVNREVDEP := $(shell echo "$(SVNREVSTR)" | diff - -q $(SVNREVFILE) >/dev/null 2>&1 || echo SVNREVFORCE)

MAIN/main.f90: $(SVNREVFILE)
	@touch -c MAIN/main.f90

.PHONY : SVNREVFORCE

SVNREVFORCE:
	@:

# if SVNREVDEP defined to SVNREVFORCE then updates SVNREVFILE
$(SVNREVFILE): $(SVNREVDEP)
	@echo ..... revision number : $(SVNREV)
	@echo "$(SVNREVSTR)" > $@

$(LDIR)/depends.make: %/depends.make: $(D_%_SRC)
	$(MAKEDEPENDS) $*

include $(LDIR)/depends.make

