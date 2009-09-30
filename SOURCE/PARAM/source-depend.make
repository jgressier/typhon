############################################################
##   PARAM library compilation

LDIR := PARAM

####### Files

# Library
PARAM_LIB = $(PRJLIB)/libt_param.a

# Modules
PARAM_MOD = MENU_AMR.$(MOD)      \
            MENU_BOCO.$(MOD)     \
            MENU_COUPLING.$(MOD) \
            MENU_GEN.$(MOD)      \
            MENU_INIT.$(MOD)     \
            MENU_INTEG.$(MOD)    \
            MENU_INVERSE.$(MOD)  \
            MENU_MESH.$(MOD)     \
            MENU_MPI.$(MOD)      \
            MENU_NUM.$(MOD)      \
            MENU_PROBE.$(MOD)    \
            MENU_SOLVER.$(MOD)   \

# Objects
PARAM_OBJ := $(PARAM_MOD:.$(MOD)=.o)  \
            def_amr.o          \
            def_boco.o         \
            def_check.o        \
            def_connect.o      \
            def_init.o         \
            def_inverse.o      \
            def_mesh.o         \
            def_mpi.o          \
            def_other.o        \
            def_output.o       \
            def_param.o        \
            def_probe.o        \
            def_project.o      \
            def_spat.o         \
            def_time.o         \
            trait_param.o      \
            trait_zoneparam.o  \
            def_coupling.o     \

D_PARAM_OBJ := $(PARAM_OBJ:%=$(PRJOBJ)/%)

D_PARAM_SRC := $(PARAM_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(PARAM_LIB)

$(PARAM_LIB): $(D_PARAM_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(PARAM_LIB)
	@touch $(PARAM_LIB) ; rm $(PARAM_LIB)
	@$(AR) ruv $(PARAM_LIB) $(D_PARAM_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(PARAM_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(PARAM_LIB) created
	@echo ---------------------------------------------------------------

PARAM_clean:
	-rm $(PARAM_LIB) $(D_PARAM_OBJ) $(PARAM_MOD) PARAM/depends.make


####### Dependencies

PARAM/depends.make: $(D_PARAM_SRC)
	(cd PARAM ; ../$(MAKEDEPENDS))

include PARAM/depends.make


