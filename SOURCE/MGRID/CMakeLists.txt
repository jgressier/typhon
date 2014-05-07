# ---------------------------------------------------------------------------\
# Description: Specify CMake directives for this directory
# ---------------------------------------------------------------------------/

# ---------------------------------------------------------------------------\
SET(KIT MGRID)
SET(KIT_LIBS
#${pName}MyRequiredLibrary
)
# ---------------------------------------------------------------------------|


# Create a library which includes the source files.
# The extension is already found.  Any number of sources could be listed here.
SET(Kit_SRCS
update_ustboco_ghostface.f90
update_ustboco_ghostcell.f90
splitquadto3x3.f90
splitquadto2x2.f90
raffin_iso_tri.f90
precalc_grad_lsq.f90
postlimit_monotonic.f90
postlimit_barth.f90
MGRID.f90
MESHALE.f90
MATFREE.f90
LIMITER.f90
integ_ustboco.f90
grid_ustpreproc.f90
grid_preproc.f90
gmres_free.f90
extractpart_grid.f90
distrib_field.f90
DEFFIELD.f90
convert_to_svm.f90
convert_to_svm_cub.f90
convert_to_svm_4wang.f90
convert_to_svm_4kris.f90
calc_gradient_gauss.f90
calc_gradient.f90
calc_gradface_svm.f90
calcboco_ust_sym.f90
calcboco_ust.f90
calcboco_ust_extrapol.f90
calcboco_ust_coupling_face.f90
calcboco_ust_coupling.f90
calcboco_gen.f90
calcboco_connect_per_match.f90
calcboco_connect_match.f90
calcboco_connect.f90
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

