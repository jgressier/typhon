############################################################
##   Compilation du code TYPHON

all: seq mpi

####### Compiler, tools and options

SYS = $(shell Util/test-systeme)
include defvar.make.$(SYS)
include defprj.make


# Option compilation (cf defvar.make)

FF   = $(FB) $(FO)

# Précision

#PREC = SP    # Choix de la double précision (SP ou DP)

####### Files and Lists

MAKEFILE= Makefile

PRGNAME = Typhon

EXTLIBS   = $(CGNSLIB) $(LAPACKLIBS)

include librairies.make

LIB1 = libt_main.a   \
       libt_xio.a    \
       libt_cgns.a   \
       libt_zone.a 

LIB2 = libt_param.a  \
       libt_vortex.a \
       libt_eqns.a   \
       libt_eqkdif.a \
       libt_mgrid.a  \
       libt_udf.a    \
       libt_mesh.a   \
       libt_pio.a    \
       libt_math.a   \
       libt_modcom.a 

LIBSEQ = $(LIB1) libt_exchseq.a $(LIB2)
LIBMPI = $(LIB1) libt_exchmpi.a $(LIB2)

D_LIBSEQ = $(LIBSEQ:%=$(PRJLIB)/%)
D_LIBMPI = $(LIBMPI:%=$(PRJLIB)/%)

VPATH = CGNS:EQNS:EQKDIF:EXCHANGE:MAIN:MGRID:MESH:MODCOM:MATH:PARAM:PIO:UDF:VORTEX:XIO:ZONE


####### Build libraries

include   MODCOM/source-depend.make 
include     MATH/source-depend.make 
include      PIO/source-depend.make
include     MESH/source-depend.make
include      UDF/source-depend.make
include    MGRID/source-depend.make
include     EQNS/source-depend.make
include   EQKDIF/source-depend.make
include EXCHANGE/source-depend.make
include   VORTEX/source-depend.make
include    PARAM/source-depend.make
include     ZONE/source-depend.make
include     CGNS/source-depend.make
include      XIO/source-depend.make
include     MAIN/source-depend.make 


####### Build main

MAIN = Obj/main.o # dependencies in MAIN/depends.make

####### Build rules

seq: $(PRGNAME)-seq
mpi: $(PRGNAME)-mpi

$(PRGNAME)-seq : $(MAIN) $(D_LIBSEQ) $(MAKEFILE)
	@echo ---------------------------------------------------------------
	@echo EDITION DE LIEN DE $(PRGNAME)-seq ---
	@echo Librairies externes : $(EXTLIBS)
	@echo Librairies TYPHON   : $(LIBSEQ)
	@$(LINKER) $(LINKFB) $(MAIN) $(D_LIBSEQ) $(EXTLIBS) -o $(PRGNAME)-seq
	@echo --- Création de $(PRGNAME)-seq terminée ---

$(PRGNAME)-mpi : $(MAIN) $(D_LIBMPI) $(MAKEFILE)
	@echo ---------------------------------------------------------------
	@echo EDITION DE LIEN DE $(PRGNAME)-mpi ---
	@echo Librairies externes : $(EXTLIBS) $(MPILIB)
	@echo Librairies TYPHON   : $(LIBMPI)
	@$(LINKER) $(LINKFB) $(MAIN) $(D_LIBMPI) $(EXTLIBS) $(MPILIB) -o $(PRGNAME)-mpi
	@echo --- Création de $(PRGNAME)-mpi terminée ---

clean:
	-rm $(PRGNAME)-seq $(PRGNAME)-mpi 
	-rm $(PRJOBJ)/*.o 
	-rm $(D_LIBMPI) $(D_LIBSEQ) 
	-rm $(PRJINC)/*.$(MOD)
	-rm *.$(MOD)


####### Dependencies

$(PRGNAME)-seq: $(MAKEFILE) 
$(PRGNAME)-mpi: $(MAKEFILE) 

# Dependances de modules

