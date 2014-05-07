# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT MATH)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
SPMAT_SDLU.f90
SPMAT_DLU.f90
SPMAT_CRS.f90
SPMAT_BDLU.f90
SPARSE_MAT.f90
solve_jacobi.f90
solve_gmres.f90
solve_cgs.f90
solve_bicgstab.f90
solve_bicg_pjacobi.f90
solve_bicg.f90
POLYLAW.f90
MATRIX.f90
MATRIX_ARRAY.f90
INTERPOL.f90
INTEGRATION.f90
dlu_sor.f90
dlu_lu.f90
dlu_jacobi.f90
dlu_gmres.f90
dlu_cgs.f90
dlu_bicg_pjacobi.f90
dlu_bicg.f90
bdlu_gmres.f90
bdlu_bicgstab.f90
bdlu_bicg.f90
)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/MODCOM)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/PIO)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/PARAM)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Base)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Math)


# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

