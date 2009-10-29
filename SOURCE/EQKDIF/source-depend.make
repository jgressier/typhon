############################################################
##   EQKDIF library compilation

LDIR := EQKDIF

####### Files

# Library
EQKDIF_LIB = $(PRJLIBDIR)/libt_eqkdif.a

# Modules
EQKDIF_MOD = EQKDIF.$(MODEXT)      \
             MATER_LOI.$(MODEXT)   \
             MATERIAU.$(MODEXT)    \
             MENU_KDIF.$(MODEXT)

# Objects
EQKDIF_OBJ := $(EQKDIF_MOD:.$(MODEXT)=.o)        \
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

libt_eqkdif.objects := $(EQKDIF_OBJ:%=$(PRJOBJDIR)/%)
libt_eqkdif.target: $(libt_eqkdif.objects)

D_EQKDIF_SRC := $(EQKDIF_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

EQKDIF_clean:
	-rm $(EQKDIF_LIB) $(libt_eqkdif.objects) $(EQKDIF_MOD) EQKDIF/depends.make


####### Dependencies

EQKDIF/depends.make: $(D_EQKDIF_SRC)
	$(MAKEDEPENDS) EQKDIF

include EQKDIF/depends.make


