############################################################
##   Compilation de la librairie ZONE

LDIR := ZONE

####### Files

ZONE_LIB = $(PRJLIB)/libt_zone.a

ZONE_MOD = BOUND.$(MOD)                  \
           DEFCAPTEURS.$(MOD)            \
           DEFFIELD.$(MOD)               \
           DEFZONE.$(MOD)                \
           ZONE_COUPLING.$(MOD)          \
           MENU_ZONECOUPLING.$(MOD) 

ZONE_OBJ := $(ZONE_MOD:.$(MOD)=.o)       \
            calc_gradient.o              \
            calc_varcons.o               \
            calc_varprim.o               \
            calcboco_ust.o               \
            calcboco_ust_coupling.o      \
            calcboco_ust_coupling_face.o \
            calcboco_ust_extrapol.o      \
            donnees_echange.o            \
            init_champ_ust.o             \
            init_connect_ust.o           \
            init_ustboco_ghostface.o     \
            interpol_onvtex.o            \
            update_champ.o


D_ZONE_OBJ := $(ZONE_OBJ:%=$(PRJOBJ)/%)

D_ZONE_SRC := $(ZONE_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(ZONE_LIB)

$(ZONE_LIB): $(D_ZONE_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(ZONE_LIB)
	@touch $(ZONE_LIB) ; rm $(ZONE_LIB)
	@$(AR) ruv $(ZONE_LIB) $(D_ZONE_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(ZONE_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(ZONE_LIB) créée
	@echo ---------------------------------------------------------------

ZONE_clean:
	-rm  $(ZONE_LIB) $(D_ZONE_OBJ) $(ZONE_MOD)

####### Dependencies

ZONE/depends.make: $(D_ZONE_SRC)
	(cd ZONE ; ../$(MAKEDEPENDS))

include ZONE/depends.make


