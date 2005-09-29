############################################################
##   Compilation du code TYPHON - Partie MAIN

LDIR := MAIN

####### Files

MAIN_LIB = $(PRJLIB)/libt_main.a

MAIN_MOD = MODWORLD.$(MOD)

MAIN_OBJ = $(MAIN_MOD:.$(MOD)=.o)   \
           accumulfluxcorr.o        \
           analyse.o                \
           analyse_zone.o           \
           build_implicit.o         \
           calc_bilan.o             \
           calc_cpl_flux.o          \
           calc_cpl_temp.o          \
           calcdifflux.o            \
           calc_volum.o             \
           calc_zonetimestep.o      \
           calcul_raccord.o         \
           capteurs.o               \
           choixcorrection.o        \
           conditions_limites.o     \
           correction.o             \
           corr_varprim.o           \
           echange.o                \
           echange_zonedata.o       \
           echange_zonematch.o      \
           explicit_step.o          \
           flux_to_res.o            \
           implicit_step.o          \
           implicit_solve.o         \
           init_boco.o              \
           init_capteurs.o          \
           init_champ.o             \
           init_connect.o           \
           init_coupling.o          \
           init_maillage.o          \
           init_world.o             \
           init_zone.o              \
           integration.o            \
           integration_cycle.o      \
           integration_grid.o       \
           integration_zone.o       \
           integration_zone_lag.o   \
           integrationmacro_zone.o  \
           lecture_maillage.o       \
           lectzone_mesh.o          \
           mpi_strategy_pre.o       \
           mpi_strategy_post.o      \
           output_result.o          \
           update_couplingboco.o      

D_MAIN_OBJ = $(MAIN_OBJ:%=$(PRJOBJ)/%)

D_MAIN_SRC := $(MAIN_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(MAIN_LIB)

$(MAIN_LIB): $(D_MAIN_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(MAIN_LIB)
	@touch $(MAIN_LIB) ; rm $(MAIN_LIB)
	@$(AR) ruv $(MAIN_LIB) $(D_MAIN_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(MAIN_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(MAIN_LIB) créée
	@echo ---------------------------------------------------------------

MAIN_clean: 
	-rm  $(MAIN_LIB) $(D_MAIN_OBJ) $(MAIN_MOD)


####### Dependencies


MAIN/depends.make: $(D_MAIN_SRC)
	(cd MAIN ; ../$(MAKEDEPENDS))

include MAIN/depends.make





