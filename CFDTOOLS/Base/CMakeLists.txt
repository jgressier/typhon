# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT Base)
SET(KIT_LIBS
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
TIMER.f90
STRING.f90
PACKET.f90
MESHPREC.f90
LIBSORT.f90
LIBRLE.f90
IO_UNIT.f90
IOCFD.f90
FTNARGS.f90
ENDIAN.f90
CONNECTIVITY.f90
CONNECT_CSR.f90
COLOR.f90
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

