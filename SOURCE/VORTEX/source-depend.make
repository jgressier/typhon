############################################################
##   VORTEX library compilation

LDIR := VORTEX

####### Files

# Library
VORTEX_LIB = $(PRJLIBDIR)/libt_vortex.a

# Modules
VORTEX_MOD = PAN2D_LIN.$(MODEXT)   \
             MENU_VORTEX.$(MODEXT) \
             VORTEX2D.$(MODEXT)

# Objects
VORTEX_OBJ = $(VORTEX_MOD:.$(MODEXT)=.o)  \
             calc_induced_velocities.o \
             def_boco_vortex.o         \
             def_init_vortex.o         \
             def_model_vortex.o        \
             fillmat_sing_effects.o    \
             get_singularity_nodes.o   \
             init_boco_vort.o          \
             init_vort_ust.o

libt_vortex.objects = $(VORTEX_OBJ:%=$(PRJOBJDIR)/%)
libt_vortex.target: $(libt_vortex.objects)

D_VORTEX_SRC := $(VORTEX_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

VORTEX_clean:
	-rm $(VORTEX_LIB) $(libt_vortex.objects) $(VORTEX_MOD) VORTEX/depends.make


####### Dependencies

VORTEX/depends.make: $(D_VORTEX_SRC) $(LDIR)/source-depend.make
	$(MAKEDEPENDS) VORTEX

include VORTEX/depends.make


