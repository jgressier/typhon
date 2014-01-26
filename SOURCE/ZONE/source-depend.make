############################################################
##   ZONE library compilation

LDIR := ZONE

####### Files

# Library
ZONE_LIB = $(PRJLIBDIR)/libt_zone.a

# Modules
ZONE_MOD = BOUND.$(MODEXT)                  \
           DEFCAPTEURS.$(MODEXT)            \
           DEFZONE.$(MODEXT)                \
           ZONE_COUPLING.$(MODEXT)          \
           MENU_ZONECOUPLING.$(MODEXT)

# Objects
ZONE_OBJ := $(ZONE_MOD:.$(MODEXT)=.o)       \
            calc_refcons.o               \
            calc_varcons.o               \
            calc_varprim.o               \
            calc_zonetimestep.o          \
            donnees_echange.o            \
            init_connect.o               \
            init_connect_grid.o          \
            init_gridfield_ust.o         \
            init_ustboco_ghostcell.o     \
            init_ustboco_ghostface.o     \
            init_ustboco_kutta.o         \
            init_ustboco_singpanel.o     \
            initzone_field.o             \
            prb_boco_field.o             \
            prb_zone_vol.o               \
            split_zone.o                 \
            update_field.o               \
            zone_preproc.o

libt_zone.objects := $(ZONE_OBJ:%=$(PRJOBJDIR)/%)
libt_zone.target: $(libt_zone.objects)

D_ZONE_SRC := $(ZONE_OBJ:%.o=$(LDIR)/%.f90)

ZONE_clean:
	-rm $(ZONE_LIB) $(libt_zone.objects) $(ZONE_MOD) ZONE/depends.make


####### Dependencies

ZONE/depends.make: $(D_ZONE_SRC)
	$(MAKEDEPENDS) ZONE

include ZONE/depends.make


