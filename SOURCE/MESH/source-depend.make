############################################################
##   MESH library compilation

LDIR := MESH

####### Files

# Library
MESH_LIB = $(PRJLIBDIR)/libt_mesh.a

# Modules
MESH_MOD = \
           GEO3D.$(MODEXT)        \
           STRMESH.$(MODEXT)      \

# Objects
MESH_OBJ = $(MESH_MOD:.$(MODEXT)=.o)  \
           build_implicit_bdlu.o   \
           build_implicit_dlu.o    \
           calc_connface.o         \
           calc_ustmesh.o          \
           init_implicit_bdlu.o    \
           init_implicit_dlu.o     \
           interpface_gradient_scal.o \
           interpface_gradient_vect.o \
           interpface_gradn_scal.o    \
           interpface_gradn_vect.o    \
           scale_mesh.o            \
           test_ustmesh.o          \

libt_mesh.objects = $(MESH_OBJ:%=$(PRJOBJDIR)/%)
libt_mesh.target: $(libt_mesh.objects)

D_MESH_SRC := $(MESH_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

MESH_clean:
	-rm $(MESH_LIB) $(libt_mesh.objects) $(MESH_MOD) MESH/depends.make


####### Dependencies

MESH/depends.make: $(D_MESH_SRC)
	$(MAKEDEPENDS) MESH

include MESH/depends.make


