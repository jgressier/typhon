############################################################
##   Compilation de la librairie MESH

LDIR := MESH

####### Files

MESH_LIB = $(PRJLIB)/libt_mesh.a

MESH_MOD = ELEMVTEX.$(MOD)     \
           GEO3D.$(MOD)        \
           GRID_CONNECT.$(MOD) \
           MESHBASE.$(MOD)     \
           STRMESH.$(MOD)      \
           TENSOR3.$(MOD)      \
           USTBOCO.$(MOD)      \
           USTMESH.$(MOD) 

MESH_OBJ = $(MESH_MOD:.$(MOD)=.o)  \
           build_implicit_bdlu.o   \
           build_implicit_dlu.o    \
           calc_connface.o         \
           calc_ust_cell.o         \
           calc_ust_elemvol.o      \
           calc_ust_midcell.o      \
           calc_ust_checkface.o    \
           calc_ust_face.o         \
           calc_ustmesh.o          \
           reorder_ustconnect.o    \
           scale_mesh.o            \
           test_ustmesh.o          \


D_MESH_OBJ = $(MESH_OBJ:%=$(PRJOBJ)/%)

D_MESH_SRC := $(MESH_OBJ:%.o=$(LDIR)/%.f90)

####### Build rules

all: $(MESH_LIB)

$(MESH_LIB): $(D_MESH_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(MESH_LIB)
	@touch $(MESH_LIB) ; rm $(MESH_LIB)
	@$(AR) ruv $(MESH_LIB) $(D_MESH_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(MESH_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(MESH_LIB) créée
	@echo ---------------------------------------------------------------

MESH_clean:
	-rm $(MESH_LIB) $(D_MESH_OBJ) $(MESH_MOD) MESH/depends.make

####### Dependencies

MESH/depends.make: $(D_MESH_SRC)
	(cd MESH ; ../$(MAKEDEPENDS))

include MESH/depends.make


