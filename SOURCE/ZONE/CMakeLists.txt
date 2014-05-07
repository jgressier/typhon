# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT ZONE)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
zone_preproc.f90
ZONE_COUPLING.f90
update_field.f90
split_zone.f90
prb_zone_vol.f90
prb_boco_field.f90
MENU_ZONECOUPLING.f90
interpol_onvtex.f90
initzone_field.f90
init_ustboco_singpanel.f90
init_ustboco_kutta.f90
init_ustboco_ghostface.f90
init_ustboco_ghostcell.f90
init_gridfield_ust.f90
init_connect_grid.f90
init_connect.f90
donnees_echange.f90
DEFZONE.f90
DEFCAPTEURS.f90
calc_zonetimestep.f90
calc_varprim.f90
calc_varcons.f90
calc_refcons.f90
BOUND.f90
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

