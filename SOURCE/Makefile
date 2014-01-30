############################################################
##   TYPHON Compilation

.SUFFIXES:

help:
	@echo "----------------------------------------------------------------"
	@echo "TYPHON compilation"
	@echo "----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> seq & mpi targets"
	@echo "  seq       -> sequential   Typhon executable 'Typhon-seq'"
	@echo "  mpi       -> MPI parallel Typhon executable 'Typhon-mpi'"
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=profil-> Profiling options"
	@echo "----------------------------------------------------------------"

all: seq mpi

####### Compiler, tools and options

include defprj.make
include ../config/arch.make
include ../config/rules.make

####### Files and Lists

MAKEFILE= Makefile

PRGNAME = Typhon

EXTLIBS   = $(CGNSLIB) $(LAPACKLIB) $(METISLIB)

# include librairies.make

LIB1 = libt_main.a   \
       libt_xio.a    \
       libt_zone.a   \
       libt_mgrid.a  \

LIB2 = libt_param.a  \
       libt_vortex.a \
       libt_eqns.a   \
       libt_eqkdif.a \
       libt_udf.a    \
       libt_mesh.a   \
       libt_pio.a    \
       libt_math.a   \
       libt_modcom.a

LIBSEQ = $(LIB1) libt_exchseq.a $(LIB2)
LIBMPI = $(LIB1) libt_exchmpi.a $(LIB2)

D_LIBSEQ = $(LIBSEQ:%=$(PRJLIBDIR)/%)
D_LIBMPI = $(LIBMPI:%=$(PRJLIBDIR)/%)

VPATH = EQNS:EQKDIF:EXCHANGE:MAIN:MGRID:MESH:MODCOM:MATH:PARAM:PIO:UDF:VORTEX:XIO:ZONE


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
include      XIO/source-depend.make
include     MAIN/source-depend.make


####### Build main

MAIN:=$(PRJOBJDIR)/main.o # dependencies in MAIN/depends.make

####### Build rules

PRGSEQNAME = $(PRGNAME).$(optext)

seq: $(PRJOBJDIR) $(PRGSEQNAME)
mpi: $(PRJOBJDIR) $(PRGNAME)-mpi

$(PRGSEQNAME) : $(MAIN) $(D_LIBSEQ) $(MAKEFILE)
	@echo "----------------------------------------------------------------"
	@echo "LINKING $(PRGNAME)-seq ---"
	@echo "external Libraries: $(PRJLIBEXT) $(EXTLIBS)"
	@echo "TYPHON   Libraries: $(LIBSEQ)"
	(cd $(PRJLIBDIR) ; $(LINKER) $(LINKFB) ../$(MAIN) $(LIBSEQ) $(PRJLIBEXT) $(EXTLIBS) -o ../$(PRGSEQNAME))
	@echo "--- link $(PRGNAME)-seq to $(PRGSEQNAME) ---"
	@ln -fs $(PRGSEQNAME) $(PRGNAME)-seq
	@echo "--- CREATION OF $(PRGSEQNAME) DONE ---"

$(PRGNAME)-mpi : LINKER = $(MPIF90C)
#$(PRGNAME)-mpi : F90OPT += $(MPIF90_FL)

$(PRGNAME)-mpi : $(MAIN) $(D_LIBMPI) $(MAKEFILE)
	@echo "----------------------------------------------------------------"
	@echo "LINKING $(PRGNAME)-mpi ---"
	@echo "external Libraries: $(PRJLIBEXT) $(EXTLIBS) $(MPILIB)"
	@echo "TYPHON   Libraries: $(LIBMPI)"
	(cd $(PRJLIBDIR) ; $(LINKER) $(LINKFB) ../$(MAIN) $(LIBMPI) $(PRJLIBEXT) $(EXTLIBS) $(MPILIB) -o ../$(PRGNAME)-mpi)
	@echo "--- CREATION OF $(PRGNAME)-mpi DONE ---"

clean:
	rm -f $(PRGNAME)-seq $(PRGNAME)-mpi
	rm -f $(PRJOBJDIR)/*.o
	rm -f $(D_LIBMPI) $(D_LIBSEQ)
	rm -f $(PRJINCDIR)/*.$(MODEXT)
	rm -f *.$(MODEXT)


####### Dependencies

$(PRGNAME)-seq: $(MAKEFILE)
$(PRGNAME)-mpi: $(MAKEFILE)

# Module dependencies

