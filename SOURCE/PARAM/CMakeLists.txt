# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT PARAM)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
trait_zoneparam.f90
trait_param.f90
MENU_SOLVER.f90
MENU_NUM.f90
MENU_MPI.f90
MENU_INVERSE.f90
MENU_INTEG.f90
MENU_INIT.f90
MENU_GEN.f90
MENU_COUPLING.f90
MENU_BOCO.f90
MENU_AMR.f90
MENU_ALE.f90
def_time.f90
def_spat.f90
def_project.f90
def_probe.f90
def_param.f90
def_output.f90
def_other.f90
def_mrf.f90
def_mpi.f90
def_mesh.f90
def_inverse.f90
def_init.f90
def_fctenv.f90
def_coupling.f90
def_connect.f90
def_check.f90
def_boco.f90
def_amr.f90
def_ale.f90
)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/MODCOM)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/SOURCE/EQNS)
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Base)

# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

