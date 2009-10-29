############################################################
##   PARAM library compilation

LDIR := PARAM

####### Files

# Library
PARAM_LIB = $(PRJLIBDIR)/libt_param.a

# Modules
PARAM_MOD = MENU_AMR.$(MODEXT)      \
            MENU_BOCO.$(MODEXT)     \
            MENU_COUPLING.$(MODEXT) \
            MENU_GEN.$(MODEXT)      \
            MENU_INIT.$(MODEXT)     \
            MENU_INTEG.$(MODEXT)    \
            MENU_INVERSE.$(MODEXT)  \
            MENU_MESH.$(MODEXT)     \
            MENU_MPI.$(MODEXT)      \
            MENU_NUM.$(MODEXT)      \
            MENU_PROBE.$(MODEXT)    \
            MENU_SOLVER.$(MODEXT)   \

# Objects
PARAM_OBJ := $(PARAM_MOD:.$(MODEXT)=.o)  \
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

libt_param.objects := $(PARAM_OBJ:%=$(PRJOBJDIR)/%)
libt_param.target: $(libt_param.objects)

D_PARAM_SRC := $(PARAM_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

PARAM_clean:
	-rm $(PARAM_LIB) $(libt_param.objects) $(PARAM_MOD) PARAM/depends.make


####### Dependencies

PARAM/depends.make: $(D_PARAM_SRC)
	$(MAKEDEPENDS) PARAM

include PARAM/depends.make


