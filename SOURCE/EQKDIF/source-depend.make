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
              accumulfluxcorr_kdif.o          \
              corr_varprim_kdif.o             \
              def_boco_kdif.o                 \
              def_init_kdif.o                 \
              def_model_kdif.o                \
              calcdifflux_kdif.o              \
              calc_kdif_flux.o                \
              calc_kdif_timestep.o            \
              calc_fluxinter_kdif.o           \
              calc_flux_fluxface.o            \
              calc_flux_fluxface_3D.o         \
              calc_flux_fluxface_consistant.o \
              calc_flux_fluxface_compact.o    \
              calc_flux_fluxspe.o             \
              calc_flux_fluxspe_3D.o          \
              calc_flux_fluxspe_consistant.o  \
              calc_flux_fluxspe_compact.o     \
              calc_fourier.o                  \
              calc_tempinter_kdif.o           \
              calc_varcons_kdif.o             \
              calc_varprim_kdif.o             \
              calcboco_kdif_ust_nunif.o       \
              calcboco_kdif_ust_unif.o        \
              echange_kdif.o                  \
              ech_data_kdif.o                 \
              init_boco_kdif.o                \
              init_coupling_kdif.o            \
              init_kdif_ust.o                 \
              integration_kdif_ust.o          \
              setboco_kdif_isoth_nunif.o      \
              setboco_kdif_isoth_unif.o       \
              stock_kdif_cond_coupling.o

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
	-rm  $(EQKDIF_LIB) $(D_EQKDIF_OBJ) $(EQKDIF_MOD)


####### Dependencies


EQKDIF/depends.make: $(D_EQKDIF_SRC)
	(cd EQKDIF ; ../$(MAKEDEPENDS))

include EQKDIF/depends.make





