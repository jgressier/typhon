############################################################
##   EQNS library compilation

LDIR := EQNS

####### Files

# Library
EQNS_LIB = $(PRJLIBDIR)/libt_eqns.a

# Modules
EQNS_MOD = EQNS.$(MODEXT)      \
           MENU_NS.$(MODEXT)

# Objects
EQNS_OBJ := $(EQNS_MOD:.$(MODEXT)=.o)    \
            calc_flux_fvs_wps.o       \
            calc_flux_fvs_hwps.o      \
            calc_flux_ausmm.o         \
            calc_flux_hlle.o          \
            calc_flux_hllc.o          \
            calc_flux_efm.o           \
            calc_flux_rusanov.o       \
            calc_flux_inviscid.o      \
            calc_flux_viscous.o       \
            calc_flux_ale.o           \
	    calc_source_ext.o         \
	    calc_source_mrf.o         \
            calc_jac_gencall.o        \
            calc_jac_hll.o            \
            calc_jac_hlldiag.o        \
            calc_jac_rusanov.o        \
            calc_jac_vlh.o            \
            calc_jacflux.o            \
            calc_jacnum_inviscid.o    \
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
            ns_bocojacobian.o         \
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

libt_eqns.objects := $(EQNS_OBJ:%=$(PRJOBJDIR)/%)
libt_eqns.target: $(libt_eqns.objects)

D_EQNS_SRC := $(EQNS_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

EQNS_clean:
	-rm $(EQNS_LIB) $(libt_eqns.objects) $(EQNS_MOD) EQNS/depends.make


####### Dependencies

EQNS/depends.make: $(D_EQNS_SRC)
	$(MAKEDEPENDS) EQNS

include EQNS/depends.make
