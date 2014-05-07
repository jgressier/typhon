# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT EQKDIF)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
stock_kdif_cond_coupling.f90
setboco_kdif_isoth.f90
setboco_kdif_hconv.f90
setboco_kdif_flux.f90
MENU_KDIF.f90
MATER_LOI.f90
MATERIAU.f90
kdif_bocoflux.f90
inverse_get_tmes.f90
int_kdif_rungekutta.f90
integration_kdif_ust.f90
init_viewfactor.f90
init_kdif_ust.f90
init_boco_kdif.f90
EQKDIF.f90
def_model_kdif.f90
def_init_kdif.f90
def_boco_kdif.f90
corr_varprim_kdif.f90
calc_varprim_kdif.f90
calc_varcons_kdif.f90
calc_kdif_timestep.f90
calc_kdif_fourier.f90
calc_kdif_flux.f90
calc_fourier.f90
calc_fouriercycle.f90
calc_flux_fluxspe.f90
calc_flux_fluxspe_consistant.f90
calc_flux_fluxspe_compact.f90
calc_flux_fluxspe_3D.f90
calc_flux_fluxface.f90
calc_flux_fluxface_consistant.f90
calc_flux_fluxface_compact.f90
calc_flux_fluxface_3D.f90
calcdifflux_kdif.f90
calcboco_kdif.f90
calcboco_kdif_coupling_face.f90
add_kdif_radiativeflux.f90
add_kdif_coupled_radflux.f90
)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
#INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/MyNeededInclude)

# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

