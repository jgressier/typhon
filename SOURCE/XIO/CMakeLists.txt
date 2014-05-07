# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT XIO)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
REPRISE.f90
outputzone_ustmesh.f90
outputzone_sol.f90
outputzone_open.f90
output_zone.f90
outputzone_close.f90
output_tec_ust_node.f90
output_tec_ust.f90
output_tec_ust_ctr.f90
output_tec_ust_boco.f90
output_tecplot.f90
comp_flux.f90
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

