############################################################
##   Compilation de la librairie PARAM

LDIR := PARAM

####### Files

PARAM_LIB = $(PRJLIB)/libt_param.a

PARAM_MOD = MENU_AMR.$(MOD)      \
            MENU_BOCO.$(MOD)     \
            MENU_CAPTEURS.$(MOD) \
            MENU_COUPLING.$(MOD) \
            MENU_GEN.$(MOD)      \
            MENU_INIT.$(MOD)     \
            MENU_INTEG.$(MOD)    \
            MENU_MESH.$(MOD)     \
            MENU_MPI.$(MOD)      \
            MENU_NUM.$(MOD)      \
            MENU_SOLVER.$(MOD)   \


PARAM_OBJ := $(PARAM_MOD:.$(MOD)=.o)  \
            def_amr.o          \
            def_boco.o         \
            def_capteurs.o     \
            def_init.o         \
            def_mesh.o         \
            def_mpi.o          \
            def_other.o        \
            def_output.o       \
            def_param.o        \
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
	@echo \* Création de la librairie $(PARAM_LIB)
	@touch $(PARAM_LIB) ; rm $(PARAM_LIB)
	@$(AR) ruv $(PARAM_LIB) $(D_PARAM_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(PARAM_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(PARAM_LIB) créée
	@echo ---------------------------------------------------------------

PARAM_clean:
	-rm  $(PARAM_LIB) $(D_PARAM_OBJ) $(PARAM_MOD) PARAM/depends.make

####### Dependencies


PARAM/depends.make: $(D_PARAM_SRC)
	(cd PARAM ; ../$(MAKEDEPENDS))

include PARAM/depends.make





