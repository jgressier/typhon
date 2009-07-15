############################################################
##   Compilation du code TYPHON

help:
	@echo "-----------------------------------------------------------------"
	@echo "TYPHON compilation"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> seq & mpi targets"
	@echo "  seq       -> sequential   Typhon executable 'Typhon-seq'"
	@echo "  mpi       -> MPI parallel Typhon executable 'Typhon-mpi'"
	@echo "Options:"
	@echo "  OPT=opt   -> Optimized options (default)"
	@echo "  OPT=debug -> Debugging options"
	@echo "  OPT=prof  -> Profiling options"
	@echo "-----------------------------------------------------------------"

all: seq mpi

####### Compiler, tools and options

include defvar.make
include defprj.make

# Précision

#PREC = SP    # Choix de la double précision (SP ou DP)

####### Files and Lists

MAKEFILE= Makefile

PRGNAME = Typhon

EXTLIBS   = $(CGNSLIB) $(LAPACKLIB) $(METISLIB)

# include librairies.make

LIB1 = libt_main.a   \
       libt_xio.a    \
       libt_zone.a   \
       libt_mgrid.a  \
       libt_cgns.a   \

LIB2 = libt_param.a  \
       libt_vortex.a \
       libt_eqns.a   \
       libt_eqkdif.a \
       libt_udf.so   \
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
	@echo LINKING $(PRGNAME)-seq ---
	@echo external Libraries: $(EXTLIBS)
	@echo TYPHON   Libraries: $(LIBSEQ)
	(cd $(PRJLIB) ; $(LINKER) $(LINKFB) ../$(MAIN) $(LIBSEQ) $(EXTLIBS) -o ../$(PRGNAME)-seq)
	@echo --- Creation of $(PRGNAME)-seq done ---

$(PRGNAME)-mpi : $(MAIN) $(D_LIBMPI) $(MAKEFILE)
	@echo ---------------------------------------------------------------
	@echo  LINKING $(PRGNAME)-mpi ---
	@echo external Libraries: $(EXTLIBS) $(MPILIB)
	@echo TYPHON   Libraries: $(LIBMPI)
	(cd $(PRJLIB) ; $(LINKER) $(LINKFB) ../$(MAIN) $(LIBMPI) $(EXTLIBS) $(MPILIB) -o ../$(PRGNAME)-mpi)
	@echo --- Creation of $(PRGNAME)-mpi done ---

clean:
	-rm -f $(PRGNAME)-seq $(PRGNAME)-mpi 
	-rm -f $(PRJOBJ)/*.o 
	-rm -f $(D_LIBMPI) $(D_LIBSEQ) 
	-rm -f $(PRJINC)/*.$(MOD)
	-rm -f *.$(MOD)


####### Dependencies

$(PRGNAME)-seq: $(MAKEFILE) 
$(PRGNAME)-mpi: $(MAKEFILE) 

# Dependances de modules

