############################################################
##   MGRID library compilation

LDIR := MGRID

####### Files

# Library
MGRID_LIB = $(PRJLIBDIR)/libt_mgrid.a

# Modules
MGRID_MOD = \
            DEFFIELD.$(MODEXT)     \
            LIMITER.$(MODEXT)      \
            MATFREE.$(MODEXT)      \
            MESHALE.$(MODEXT)      \
            MGRID.$(MODEXT)

# Objects
MGRID_OBJ = $(MGRID_MOD:.$(MODEXT)=.o)      \
            calcboco_connect.o           \
            calcboco_connect_match.o     \
            calcboco_connect_per_match.o \
            calcboco_ust.o               \
            calcboco_ust_extrapol.o      \
            calcboco_ust_sym.o           \
            calc_gradient.o            \
            calc_gradient_limite.o     \
            calc_gradient_gauss.o      \
            calc_gradface_svm.o        \
            convert_to_svm.o           \
            convert_to_svm_cub.o       \
            convert_to_svm_4wang.o     \
            convert_to_svm_4kris.o     \
            raffin_iso_tri.o           \
            raffin_iso_quad.o          \
            distrib_field.o            \
            extractpart_grid.o         \
            getpart_grid.o             \
            grid_preproc.o             \
            grid_ustpreproc.o          \
            gmres_free.o               \
            integ_ustboco.o            \
            postlimit_monotonic.o      \
            postlimit_barth.o          \
            precalc_grad_lsq.o         \
            update_ustboco_ghostcell.o \
            update_ustboco_ghostface.o
            #minmax_limiter.o

libt_mgrid.objects = $(MGRID_OBJ:%=$(PRJOBJDIR)/%)
libt_mgrid.target: $(libt_mgrid.objects)

D_MGRID_SRC := $(MGRID_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

MGRID_clean:
	-rm $(MGRID_LIB) $(libt_mgrid.objects) $(MGRID_MOD) MGRID/depends.make


####### Dependencies

MGRID/depends.make: $(D_MGRID_SRC)
	$(MAKEDEPENDS) MGRID

include MGRID/depends.make


