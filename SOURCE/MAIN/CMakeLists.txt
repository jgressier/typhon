# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT MAIN)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
write_monitors_iteration.f90
write_monitors_cycle.f90
write_bocohisto.f90
update_couplingboco.f90
tstep_implicit.f90
treelevel_rungekutta.f90
treelevel_explicit.f90
readallmesh.f90
output_result.f90
mpi_strategy_pre.f90
mpi_strategy_post.f90
MODWORLD.f90
main.f90
inverse_calc_sensi.f90
integzone_tstep_lagrange.f90
integzone_tstep_gridtree.f90
integ_treelevel.f90
integration_grid.f90
integration.f90
integration_cyclezone.f90
integration_cycle_inverse.f90
integration_cycle.f90
init_zone.f90
init_world.f90
init_probes.f90
init_inverse.f90
init_implicit.f90
init_coupling.f90
init_bocohisto.f90
init_boco.f90
implicit_solve.f90
flux_to_res.f90
echange_zonematch.f90
echange_zonedata.f90
echange.f90
crit_calc_cycle.f90
corr_varprim.f90
correction.f90
choixcorrection.f90
check_end_cycle.f90
calc_volum.f90
calcul_raccord.f90
calc_rhs.f90
calc_hres_states.f90
calcdifflux.f90
calc_cpl_temp.f90
calc_cpl_flux.f90
build_implicit.f90
analyse_zone.f90
analyse.f90
accumulfluxcorr.f90
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

