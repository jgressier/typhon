############################################################
##   EQNS library compilation

LDIR := EQNS

####### Files

# Library
EQNS_LIB = $(PRJLIB)/libt_eqns.a

# Modules
EQNS_MOD = EQNS.$(MOD)      \
           MENU_NS.$(MOD)

# Objects
EQNS_OBJ := $(EQNS_MOD:.$(MOD)=.o)    \
            calc_flux_ausmm.o         \
            calc_flux_hlle.o          \
            calc_flux_hllc.o          \
            calc_flux_efm.o           \
            calc_flux_rusanov.o       \
            calc_flux_inviscid.o      \
            calc_flux_viscous.o       \
            calc_jac_hll.o            \
            calc_jac_hlldiag.o        \
            calc_jac_rusanov.o        \
            calc_jacflux.o            \
            calc_ns_timestep.o        \
            calc_roe_states.o         \
            calc_varcons_ns.o         \
            calc_varprim_ns.o         \
            calcboco_ns.o             \
            def_boco_ns.o             \
            def_init_ns.o             \
            def_model_ns.o            \
            hres_ns_muscl.o           \
            hres_ns_musclfast.o       \
            hres_ns_muscluns.o        \
            hres_ns_svm.o             \
            init_boco_ns.o            \
            init_ns_ust.o             \
            integration_ns_ust.o      \
            ns_bocoflux.o             \
            setboco_ns_flux.o         \
            setboco_ns_hconv.o        \
            setboco_ns_inlet_sub.o    \
            setboco_ns_inlet_sup.o    \
            setboco_ns_isoth.o        \
            setboco_ns_outlet_sub.o   \
            setboco_ns_outlet_sup.o   \
            stock_ns_cond_coupling.o  \
            tvdgradfst_scal.o         \
            tvdgradstr_scal.o         \
            tvdgraduns_scal.o         \
            tvdgradfst_vect.o         \
            tvdgradstr_vect.o         \
            tvdgraduns_vect.o         \

D_EQNS_OBJ := $(EQNS_OBJ:%=$(PRJOBJ)/%)

D_EQNS_SRC := $(EQNS_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(EQNS_LIB)

$(EQNS_LIB): $(D_EQNS_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(EQNS_LIB)
	@touch $(EQNS_LIB) ; rm $(EQNS_LIB)
	@$(AR) ruv $(EQNS_LIB) $(D_EQNS_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(EQNS_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(EQNS_LIB) created
	@echo ---------------------------------------------------------------

EQNS_clean:
	-rm $(EQNS_LIB) $(D_EQNS_OBJ) $(EQNS_MOD) EQNS/depends.make


####### Dependencies

EQNS/depends.make: $(D_EQNS_SRC)
	(cd EQNS ; ../$(MAKEDEPENDS))

include EQNS/depends.make


