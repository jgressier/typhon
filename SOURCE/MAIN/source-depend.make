############################################################
##   Compilation du code TYPHON - Partie MAIN

LDIR := MAIN

####### Files

MAIN_LIB = $(PRJLIB)/libt_main.a

MAIN_MOD = MODINFO.$(MOD)      \
           MODWORLD.$(MOD)

MAIN_OBJ = $(MAIN_MOD:.$(MOD)=.o)   \
           accumulfluxcorr.o        \
           calc_bilan.o             \
           calcdifflux.o            \
           calc_volum.o             \
           calc_zonetimestep.o      \
           calcul_raccord.o         \
           capteurs.o               \
           comp_flux.o              \
           conditions_limites.o     \
           corr_varprim.o           \
           echange.o                \
           echange_zonedata.o       \
           echange_zonematch.o      \
           init_champ.o             \
           init_connect.o           \
           init_coupling.o          \
           init_maillage.o          \
           init_world.o             \
           integration.o            \
           integration_macrodt.o    \
           integration_strdomaine.o \
           integration_ustdomaine.o \
           integration_zone.o       \
           integrationmacro_zone.o  \
           lecture_maillage.o       \
           lectzone_mesh.o          \
           main.o                   \
           output_result.o                

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





