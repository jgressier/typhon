############################################################
##   Compilation de la librairie VORTEX

LDIR := VORTEX

####### Files

VORTEX_LIB = $(PRJLIB)/libt_vortex.a

VORTEX_MOD = PAN2D_LIN.$(MOD)   \
             MENU_VORTEX.$(MOD) \
             VORTEX2D.$(MOD)  


VORTEX_OBJ = $(VORTEX_MOD:.$(MOD)=.o)  \
             calc_induced_velocities.o \
             def_boco_vortex.o         \
             def_init_vortex.o         \
             def_model_vortex.o        \
             fillmat_sing_effects.o    \
             get_singularity_nodes.o   \
             init_boco_vort.o          \
             init_vort_ust.o


D_VORTEX_OBJ = $(VORTEX_OBJ:%=$(PRJOBJ)/%)

D_VORTEX_SRC := $(VORTEX_OBJ:%.o=$(LDIR)/%.f90)

####### Build rules

all: $(VORTEX_LIB)

$(VORTEX_LIB): $(D_VORTEX_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(VORTEX_LIB)
	@touch $(VORTEX_LIB) ; rm $(VORTEX_LIB)
	@$(AR) ruv $(VORTEX_LIB) $(D_VORTEX_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(VORTEX_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(VORTEX_LIB) créée
	@echo ---------------------------------------------------------------

VORTEX_clean:
	-rm $(VORTEX_LIB) $(D_VORTEX_OBJ) $(VORTEX_MOD)

####### Dependencies

VORTEX/depends.make: $(D_VORTEX_SRC) $(LDIR)/source-depend.make
	(cd VORTEX ; $(SHELL) ../$(MAKEDEPENDS))

include VORTEX/depends.make


