############################################################
##   Compilation du code TYPHON

####### Compiler, tools and options

SYS = $(shell Util/test-systeme)
include defvar.make.$(SYS)
include defprj.make


# Option compilation (cf defvar.make)

FF   = $(FB) $(FO)

# Précision

PREC = SP    # Choix de la double précision (SP ou DP)

####### Files and Lists

MAKEFILE= Makefile

PRGNAME = Typhon

all: $(PRGNAME)

EXTLIBS   = $(LAPACKLIBS)

include librairies.make

#LIBRARIES = libcgns.a libt_modcom.a libt_pio.a libt_eqkdif.a libt_param.a libt_cgns.a libt_mesh.a libt_zone.a libt_xio.a  libt_main.a   # libt_eqns.a  \

LIBRARIES = libt_main.a libt_xio.a    \
            libt_cgns.a               \
            libt_zone.a libt_mesh.a   \
            libt_param.a              \
            libt_eqns.a               \
            libt_eqkdif.a             \
            libt_pio.a libt_modcom.a  \
            libcgns.a 

D_LIBRARIES = $(LIBRARIES:%=$(PRJLIB)/%)

VPATH = CGNS:EQNS:EQKDIF:MAIN:MESH:MODCOM:MODCOM/MATH:PARAM:PIO:XIO:ZONE


####### Build libraries

include MODCOM/source-depend.make 
include    PIO/source-depend.make
include   EQNS/source-depend.make
include EQKDIF/source-depend.make
include  PARAM/source-depend.make
include   MESH/source-depend.make
include   ZONE/source-depend.make
include   CGNS/source-depend.make
include    XIO/source-depend.make
include   MAIN/source-depend.make 


####### Build rules

all: $(PRGNAME)


$(PRGNAME) : $(D_LIBRARIES) $(MAKEFILE)
	@echo ---------------------------------------------------------------
	@echo EDITION DE LIEN DE $(PRGNAME) ---
	@echo Librairies externes : $(EXTLIBS)
	@echo Librairies TYPHON   : $(LIBRARIES)
	@$(LINKER) -L/usr/lib $(LINKFB) $(D_LIBRARIES) $(EXTLIBS) -o $(PRGNAME)
	@echo --- Création de $(PRGNAME) terminée ---

clean:  CGNS_clean  MAIN_clean  MESH_clean  MODCOM_clean  PARAM_clean  \
        PIO_clean  XIO_clean  ZONE_clean  EQKDIF_clean EQNS_clean
	-rm $(PRGNAME) $(OBJECTS) $(D_LIBRARIES) $(PRJINC)/*.$(MOD)


####### Dependencies

$(PRGNAME): $(MAKEFILE) 

# Dependances de modules

