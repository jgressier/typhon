############################################################
##   CGNS library compilation

LDIR := CGNS

####### Files

# Library
CGNS_LIB = $(PRJLIBDIR)/libt_cgns.a

# Modules
CGNS_MOD = CGNS_STRUCT.$(MODEXT)       \
           CGNSLIB.$(MODEXT)

# Objects
CGNS_OBJ := $(CGNS_MOD:.$(MODEXT)=.o)   \
            cgns2typhon_zone.o       \
            cgns2typhon_ustboco.o    \
            cgns2typhon_ustmesh.o    \
            createface_fromcgns.o    \
            readcgns_strboco.o       \
            readcgns_strconnect.o    \
            readcgns_ustboco.o       \
            readcgns_ustconnect.o    \
            readcgnsbase.o           \
            readcgnsfile.o           \
            readcgnsvtex.o           \
            readcgnszone.o           \
            readcgns_sol.o           \
            seek_bcface_face.o       \
            seek_bcface_vtex.o       \
	    writecgns_bocomesh.o     \
	    writecgns_ustmesh.o      \
	    writecgns_sol.o          \
	    writecgns_zone.o

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


