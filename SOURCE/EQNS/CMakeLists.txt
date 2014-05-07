# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT EQNS)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
tvdgraduns_vect.f90
tvdgraduns_scal.f90
tvdgradstr_vect.f90
tvdgradstr_scal.f90
tvdgradfst_vect.f90
tvdgradfst_scal.f90
stock_ns_cond_coupling.f90
setboco_ns_outlet_sup.f90
setboco_ns_outlet_sub.f90
setboco_ns_isoth.f90
setboco_ns_inlet_sup.f90
setboco_ns_inlet_sub.f90
setboco_ns_hconv.f90
setboco_ns_flux.f90
ns_bocojacobian.f90
ns_bocoflux.f90
MENU_NS.f90
integration_ns_ust.f90
init_ns_ust.f90
init_boco_ns.f90
hres_ns_svm.f90
hres_ns_muscluns.f90
hres_ns_musclfast.f90
hres_ns_muscl.f90
EQNS.f90
def_model_ns.f90
def_init_ns.f90
def_boco_ns.f90
calc_wallvelocity.f90
calc_varprim_ns.f90
calc_varcons_ns.f90
calc_source_mrf.f90
calc_source_ext.f90
calc_source_axisym.f90
calc_roe_states.f90
calc_ns_timestep.f90
calc_jac_vlh.f90
calc_jac_rusanov.f90
calc_jacnum_inviscid.f90
calc_jac_hll.f90
calc_jac_hlldiag.f90
calc_jac_gencall.f90
calc_jacflux.f90
calc_flux_viscous_face.f90
calc_flux_viscous_cell.f90
calc_flux_rusanov.f90
calc_flux_inviscid.f90
calc_flux_hlle.f90
calc_flux_hllc.f90
calc_flux_fvs_wps.f90
calc_flux_fvs_hwps.f90
calc_flux_efm.f90
calc_flux_ausmm.f90
calc_flux_ale.f90
calcboco_ns.f90
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

