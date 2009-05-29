############################################################
##   Compilation de la librairie ZONE

LDIR := ZONE

####### Files

ZONE_LIB = $(PRJLIB)/libt_zone.a

ZONE_MOD = BOUND.$(MOD)                  \
           DEFCAPTEURS.$(MOD)            \
           DEFZONE.$(MOD)                \
           ZONE_COUPLING.$(MOD)          \
           MENU_ZONECOUPLING.$(MOD) 

ZONE_OBJ := $(ZONE_MOD:.$(MOD)=.o)       \
            calc_varcons.o               \
            calc_varprim.o               \
            donnees_echange.o            \
            init_connect_grid.o          \
            init_gridfield_ust.o         \
            init_ustboco_ghostcell.o     \
            init_ustboco_ghostface.o     \
            init_ustboco_kutta.o         \
            init_ustboco_singpanel.o     \
            prb_boco_field.o             \
            split_zone.o                 \
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
	-rm  $(ZONE_LIB) $(D_ZONE_OBJ) $(ZONE_MOD) ZONE/depends.make

####### Dependencies

ZONE/depends.make: $(D_ZONE_SRC)
	(cd ZONE ; ../$(MAKEDEPENDS))

include ZONE/depends.make


