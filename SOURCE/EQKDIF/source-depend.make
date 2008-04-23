############################################################
##   Compilation de la librairie MODZONE

LDIR := EQKDIF

####### Files

EQKDIF_LIB = $(PRJLIB)/libt_eqkdif.a

EQKDIF_MOD = EQKDIF.$(MOD)      \
             MATER_LOI.$(MOD)   \
             MATERIAU.$(MOD)    \
             MENU_KDIF.$(MOD)


EQKDIF_OBJ := $(EQKDIF_MOD:.$(MOD)=.o)        \
              add_kdif_radiativeflux.o        \
              add_kdif_coupled_radflux.o      \
              corr_varprim_kdif.o             \
              def_boco_kdif.o                 \
              def_init_kdif.o                 \
              def_model_kdif.o                \
              calcdifflux_kdif.o              \
              calc_kdif_flux.o                \
              calc_kdif_fourier.o             \
              calc_kdif_timestep.o            \
              calc_flux_fluxface.o            \
              calc_flux_fluxface_3D.o         \
              calc_flux_fluxface_consistant.o \
              calc_flux_fluxface_compact.o    \
              calc_flux_fluxspe.o             \
              calc_flux_fluxspe_3D.o          \
              calc_flux_fluxspe_consistant.o  \
              calc_flux_fluxspe_compact.o     \
              calc_fourier.o                  \
              calc_fouriercycle.o             \
              calc_varcons_kdif.o             \
              calc_varprim_kdif.o             \
              calcboco_kdif.o                 \
              kdif_bocoflux.o                 \
              init_boco_kdif.o                \
              init_kdif_ust.o                 \
              init_viewfactor.o               \
              inverse_get_tmes.o              \
              integration_kdif_ust.o          \
              setboco_kdif_flux.o             \
              setboco_kdif_hconv.o            \
              setboco_kdif_isoth.o            \
              stock_kdif_cond_coupling.o      \

D_EQKDIF_OBJ := $(EQKDIF_OBJ:%=$(PRJOBJ)/%)

D_EQKDIF_SRC := $(EQKDIF_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(EQKDIF_LIB)

$(EQKDIF_LIB): $(D_EQKDIF_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(EQKDIF_LIB)
	@touch $(EQKDIF_LIB) ; rm $(EQKDIF_LIB)
	@$(AR) ruv $(EQKDIF_LIB) $(D_EQKDIF_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(EQKDIF_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(EQKDIF_LIB) créée
	@echo ---------------------------------------------------------------

EQKDIF_clean:
	-rm  $(EQKDIF_LIB) $(D_EQKDIF_OBJ) $(EQKDIF_MOD) EQKDIF/depends.make


####### Dependencies


EQKDIF/depends.make: $(D_EQKDIF_SRC)
	(cd EQKDIF ; ../$(MAKEDEPENDS))

include EQKDIF/depends.make





