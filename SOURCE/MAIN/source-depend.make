############################################################
##   Compilation du code TYPHON - Partie MAIN

LDIR := MAIN

####### Files

MAIN_LIB = libt_main.a

MAIN_MOD = MODINFO.$(MOD)      \
           MODWORLD.$(MOD)

MAIN_OBJ = $(MAIN_MOD:.$(MOD)=.o)   \
           calc_bilan.o             \
           calc_flux_euler.o        \
           calc_volum.o             \
           calcul_raccord.o         \
           capteurs.o               \
           comp_flux.o              \
           conditions_limites.o     \
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

all: $(PRJLIB)/$(MAIN_LIB)

$(PRJLIB)/$(MAIN_LIB): $(MAIN_LIB)
	@echo \* Copie de la librairie $(MAIN_LIB)
	@cp $(MAIN_LIB) $(PRJLIB)

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





