############################################################
##   CGNS library compilation

LDIR := CGNS

####### Files

# Library
CGNS_LIB = $(PRJLIBDIR)/libt_cgns.a

# Modules
CGNS_MOD = \
           CGNSLIB.$(MODEXT)

# Objects
CGNS_OBJ := $(CGNS_MOD:.$(MODEXT)=.o)   \
            cgns2typhon_zone.o       \
            cgns2typhon_ustboco.o    \
            cgns2typhon_ustmesh.o    \
            createface_fromcgns.o    \
            readcgns_strboco.o       \
            readcgns_strconnect.o    \
            readcgnsbase.o           \
            readcgnsfile.o           \
            seek_bcface_face.o       \
            seek_bcface_vtex.o       \

libt_cgns.objects := $(CGNS_OBJ:%=$(PRJOBJDIR)/%)
libt_cgns.target: $(libt_cgns.objects)

D_CGNS_SRC := $(CGNS_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

CGNS_clean:
	-rm $(CGNS_LIB) $(libt_cgns.objects) $(CGNS_MOD) CGNS/depends.make


####### Dependencies

CGNS/depends.make: $(D_CGNS_SRC)
	$(MAKEDEPENDS) CGNS

include CGNS/depends.make


