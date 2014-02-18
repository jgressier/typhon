############################################################
##   ZONE library compilation
#
.SUFFIXES:

# Directory
LDIR := ZONE

# Library name
LIBNAME := libt_zone

####### Files

# Library
$(LDIR).libfile := $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

# List of f90 modules
$(LDIR).f90MODFILES := \
    BOUND.f90                  \
    DEFCAPTEURS.f90            \
    DEFZONE.f90                \
    ZONE_COUPLING.f90          \
    MENU_ZONECOUPLING.f90      \

$(LDIR)_MOD := $($(LDIR).f90MODFILES:%.f90=%.$(MODEXT))

# List of f90 files
$(LDIR).f90files := \
    $($(LDIR).f90MODFILES) \
    calc_refcons.f90               \
    calc_varcons.f90               \
    calc_varprim.f90               \
    calc_zonetimestep.f90          \
    donnees_echange.f90            \
    init_connect.f90               \
    init_connect_grid.f90          \
    init_gridfield_ust.f90         \
    init_ustboco_ghostcell.f90     \
    init_ustboco_ghostface.f90     \
    init_ustboco_kutta.f90         \
    init_ustboco_singpanel.f90     \
    initzone_field.f90             \
    prb_boco_field.f90             \
    prb_zone_vol.f90               \
    split_zone.f90                 \
    update_field.f90               \
    zone_preproc.f90               \

$(LDIR).f90names := $(notdir $($(LDIR).f90files))

$(LDIR).objnames := $($(LDIR).f90names:%.f90=%.o)

$(LDIR).objfiles := $($(LDIR).objnames:%=$(PRJOBJDIR)/%)

$(LIBNAME).objfiles := $($(LDIR).objfiles)

##GG:>>>
##GG: dependency removed
##$(LIBNAME).target: $($(LDIR).objfiles)
##GG: and replaced
$($(LDIR).libfile): $($(LDIR).objfiles)
##GG:<<<

D_$(LDIR)_SRC := $($(LDIR).objnames:%.o=$(LDIR)/%.f90)


####### Build rules

$(LDIR)_clean: %_clean:
	-rm $($*.libfile) $($*.objfiles) $($*_MOD) $*/depends.make


####### Dependencies

$(LDIR)/depends.make: %/depends.make: $(D_%_SRC)
	$(MAKEDEPENDS) $*

include $(LDIR)/depends.make

