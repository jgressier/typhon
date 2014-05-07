# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT MESH)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
test_ustmesh.f90
scale_mesh.f90
interpface_gradn_vect.f90
interpface_gradn_scal.f90
interpface_gradient_vect.f90
interpface_gradient_scal.f90
init_implicit_dlu.f90
init_implicit_bdlu.f90
GEO3D.f90
extract_pts_index.f90
extract_points.f90
calc_ustmesh.f90
calc_ust_checkface.f90
calc_connface.f90
calc_cellvtex.f90
verify_cellvtex.f90
build_implicit_dlu.f90
build_implicit_bdlu.f90
)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Base)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Math)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Mesh)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/MODCOM)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/PARAM)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/PIO)

# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

