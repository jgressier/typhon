############################################################
##   Compilation de la librairie EQNS

LDIR := EQNS

####### Files

EQNS_LIB = $(PRJLIB)/libt_eqns.a

EQNS_MOD = EQNS.$(MOD)      \
           MENU_NS.$(MOD)


EQNS_OBJ := $(EQNS_MOD:.$(MOD)=.o)    \
            def_boco_ns.o             \
            def_init_ns.o             \
            def_model_ns.o            \
            calc_ns_flux.o            \
            calc_ns_timestep.o        \
            calc_varcons_ns.o         \
            calc_varprim_ns.o         \
            init_ns_ust.o 

D_EQNS_OBJ := $(EQNS_OBJ:%=$(PRJOBJ)/%)

D_EQNS_SRC := $(EQNS_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(EQNS_LIB)

$(EQNS_LIB): $(D_EQNS_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(EQNS_LIB)
	@touch $(EQNS_LIB) ; rm $(EQNS_LIB)
	@$(AR) ruv $(EQNS_LIB) $(D_EQNS_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(EQNS_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(EQNS_LIB) créée
	@echo ---------------------------------------------------------------

EQNS_clean:
	-rm  $(EQNS_LIB) $(D_EQNS_OBJ) $(EQNS_MOD)


####### Dependencies


EQNS/depends.make: $(D_EQNS_SRC)
	(cd EQNS ; ../$(MAKEDEPENDS))

include EQNS/depends.make





