############################################################
##   TYPHON code compilation - MAIN

LDIR := MAIN

####### Files

# Library
MAIN_LIB = $(PRJLIBDIR)/libt_main.a

# Modules
MAIN_MOD = MODWORLD.$(MODEXT)

# Objects
MAIN_OBJ = $(MAIN_MOD:.$(MODEXT)=.o)   \
           accumulfluxcorr.o        \
           analyse.o                \
           analyse_zone.o           \
           build_implicit.o         \
           calc_cpl_flux.o          \
           calc_cpl_temp.o          \
           calc_hres_states.o       \
           calc_rhs.o               \
           calcdifflux.o            \
           calcul_raccord.o         \
           check_end_cycle.o        \
           choixcorrection.o        \
           correction.o             \
           corr_varprim.o           \
           echange.o                \
           echange_zonedata.o       \
           echange_zonematch.o      \
           flux_to_res.o            \
           implicit_solve.o         \
           init_boco.o              \
           init_bocohisto.o         \
           init_connect.o           \
           init_coupling.o          \
           init_implicit.o          \
           init_inverse.o           \
           init_probes.o            \
           init_world.o             \
           init_zone.o              \
           integration.o            \
           integration_cycle.o      \
           integration_cycle_inverse.o \
           integration_cyclezone.o  \
           integration_grid.o       \
           integzone_tstep_lagrange.o \
           integzone_tstep_usttree.o  \
           integ_treelevel.o        \
           inverse_calc_sensi.o     \
           mpi_strategy_pre.o       \
           mpi_strategy_post.o      \
           output_result.o          \
           readallmesh.o            \
           treelevel_explicit.o     \
           treelevel_rungekutta.o   \
           tstep_implicit.o         \
           update_couplingboco.o    \
           write_bocohisto.o        \
           write_monitors_cycle.o   \
           write_monitors_iteration.o \

libt_main.objects = $(MAIN_OBJ:%=$(PRJOBJDIR)/%)
libt_main.target: $(libt_main.objects)

D_MAIN_SRC := $(MAIN_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

MAIN_clean:
	-rm $(MAIN_LIB) $(libt_main.objects) $(MAIN_MOD) MAIN/depends.make


####### Dependencies

# SVNREV : version number
SVNREV=$(shell svnversion 2> /dev/null || echo unknown)

# SVNREVSTR : fortran code line defining the string for the version number
SVNREVSTR=character(len=20), parameter :: svnrev = '$(SVNREV)'

# SVNREVFILE : name of the file which sould contain SVNREVSTR
SVNREVFILE=Include/svnrev.h

# SVNREVDEP : defined to SVNREVFORCE if SVNREVFILE does not contain SVNREVSTR
SVNREVDEP=$(shell echo "$(SVNREVSTR)" | diff - -q $(SVNREVFILE) >/dev/null 2>&1 || echo SVNREVFORCE)

MAIN/depends.make: $(D_MAIN_SRC)
	$(MAKEDEPENDS) MAIN

MAIN/main.f90: $(SVNREVFILE)
	@touch MAIN/main.f90

.PHONY : SVNREVFORCE

SVNREVFORCE:
	@:

# if SVNREVDEP define to SVNREVFORCE then updates SVNREVFILE
$(SVNREVFILE): $(SVNREVDEP)
	@echo ..... revision number : $(SVNREV)
	@echo "$(SVNREVSTR)" > $@

include MAIN/depends.make


