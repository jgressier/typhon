# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT Math)
SET(KIT_LIBS
${pName}Base 
#${pName}${KIT}
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
VEC3D.f90
VEC2D.f90
MATH.f90
FCT_PARSER.f90
FCT_NODE.f90
FCT_MATH.f90
FCT_FUNC.f90
FCT_EVAL.f90
FCT_ENV.f90
FCT_DEF.f90
FCT_CONTAINER.f90
check_fct_parser.f90
check_fct_memoryleak.f90
check_fct_eval.f90
check_fct_env.f90
check_fct_cpu.f90
)
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
# Make sure the compiler can find modules files
INCLUDE_DIRECTORIES (${PROJECT_BINARY_DIR}/CFDTOOLS/Base)

# Make sure the linker can find this library once it is built.
LINK_DIRECTORIES ({${PROJECT_NAME}_BINARY_DIR}/${KIT})
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
ADD_LIBRARY(${pName}${KIT} ${Kit_SRCS})
TARGET_LINK_LIBRARIES(${pName}${KIT} ${KIT_LIBS})
# ---------------------------------------------------------------------------/

