# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT Mesh)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
USTMESH.f90
TENSOR3.f90
STRMESH.f90
seek_bcface_vtex.f90
seek_bcface_face.f90
PROBECALC.f90
MESHPART.f90
MESHPARAMS.f90
MESHMRF.f90
MESHGEOM.f90
MESHCONNECT.f90
MESHBASE.f90
GRID_CONNECT.f90
ELEMVTEX.f90
DEF_USTBOCO.f90
DEFPROBE.f90
create_facevtex.f90
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

ADD_SUBDIRECTORY(Field)
