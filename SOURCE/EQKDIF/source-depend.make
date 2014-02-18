############################################################
##   EQKDIF library compilation
#
.SUFFIXES:

# Directory
LDIR := EQKDIF

# Library name
LIBNAME := libt_eqkdif

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    EQKDIF.f90       \
    MATER_LOI.f90    \
    MATERIAU.f90     \
    MENU_KDIF.f90    \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    add_kdif_radiativeflux.f90        \
    add_kdif_coupled_radflux.f90      \
    corr_varprim_kdif.f90             \
    def_boco_kdif.f90                 \
    def_init_kdif.f90                 \
    def_model_kdif.f90                \
    calcdifflux_kdif.f90              \
    calc_kdif_flux.f90                \
    calc_kdif_fourier.f90             \
    calc_kdif_timestep.f90            \
    calc_flux_fluxface.f90            \
    calc_flux_fluxface_3D.f90         \
    calc_flux_fluxface_consistant.f90 \
    calc_flux_fluxface_compact.f90    \
    calc_flux_fluxspe.f90             \
    calc_flux_fluxspe_3D.f90          \
    calc_flux_fluxspe_consistant.f90  \
    calc_flux_fluxspe_compact.f90     \
    calc_fourier.f90                  \
    calc_fouriercycle.f90             \
    calc_varcons_kdif.f90             \
    calc_varprim_kdif.f90             \
    calcboco_kdif.f90                 \
    kdif_bocoflux.f90                 \
    init_boco_kdif.f90                \
    init_kdif_ust.f90                 \
    init_viewfactor.f90               \
    inverse_get_tmes.f90              \
    integration_kdif_ust.f90          \
    setboco_kdif_flux.f90             \
    setboco_kdif_hconv.f90            \
    setboco_kdif_isoth.f90            \
    stock_kdif_cond_coupling.f90      \

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

##GG:>>>
##GG: dependency removed
##$(LIBNAME).target: $($(LDIR).objfiles)
##GG: and replaced
$($(LDIR).libfile): $($(LDIR).objfiles)
##GG:<<<

D_$(LDIR)_SRC := $($(LDIR).objnames:%.o=$(LDIR)/%.f90)


####### Build rules

$(LDIR)_clean: %_clean:
	-rm $($*.libfile) $($*.objfiles) $($*_MOD) $*/depends.make


####### Dependencies

$(LDIR)/depends.make: %/depends.make: $(D_%_SRC)
	$(MAKEDEPENDS) $*

include $(LDIR)/depends.make

