############################################################
##   CGNS library compilation

LDIR := CGNS

####### Files

# Library
CGNS_LIB = $(PRJLIB)/libt_cgns.a

# Modules
CGNS_MOD = CGNS_STRUCT.$(MOD)       \
           CGNSLIB.$(MOD)

# Objects
CGNS_OBJ := $(CGNS_MOD:.$(MOD)=.o)   \
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

D_CGNS_OBJ := $(CGNS_OBJ:%=$(PRJOBJ)/%)

D_CGNS_SRC := $(CGNS_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(CGNS_LIB)

$(CGNS_LIB): $(D_CGNS_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(CGNS_LIB)
	@touch $(CGNS_LIB) ; rm $(CGNS_LIB)
	@$(AR) ruv $(CGNS_LIB) $(D_CGNS_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(CGNS_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(CGNS_LIB) created
	@echo ---------------------------------------------------------------

CGNS_clean:
	-rm $(CGNS_LIB) $(D_CGNS_OBJ) $(CGNS_MOD) CGNS/depends.make


####### Dependencies

CGNS/depends.make: $(D_CGNS_SRC)
	(cd CGNS ; ../$(MAKEDEPENDS))

include CGNS/depends.make


