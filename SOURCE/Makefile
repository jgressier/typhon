############################################################
##   TYPHON Compilation

TARGETS = seq omp mpi

PRG.TARGETS = $(addprefix PRG.,$(TARGETS))

clean.TARGETS = $(addprefix clean.,$(TARGETS))

.PHONY: all $(TARGETS) $(PRG.TARGETS) clean.all $(clean.TARGETS)

.SUFFIXES:

PRGBASE = typhon

help:
	@echo "----------------------------------------------------------------"
	@echo "TYPHON compilation"
	@echo "----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> seq, omp & mpi targets"
	@echo "  seq       -> sequential      $(PRGBASE) executable '$(PRGBASE)-seq'"
	@echo "  omp       -> OpenMP parallel $(PRGBASE) executable '$(PRGBASE)-omp'"
	@echo "  mpi       -> MPI    parallel $(PRGBASE) executable '$(PRGBASE)-mpi'"
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=profil-> Profiling options"
	@echo "----------------------------------------------------------------"

ifneq ($(filter-out $(TARGETS),$(PRGEXT)),)
$(error PRGEXT = "$(PRGEXT)" must belong to ( $(TARGETS:%="%") ))
endif

all: $(TARGETS)

$(TARGETS):
	$(MAKE) PRG.$@ opt=$(opt) PRGEXT=$@

ifneq ($(filter $(MAKECMDGOALS),$(PRG.TARGETS)),)
ifneq ($(MAKECMDGOALS),PRG.$(PRGEXT))
$(error target = "$(MAKECMDGOALS)" != "PRG.$(PRGEXT)" = PRG.$$(PRGEXT))
endif
endif

####### Compiler, tools and options

PRJDIR = .

include $(PRJDIR)/defprj.make
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

####### Files and Lists

MAKEFILE= Makefile

EXTLIBS   = $(CGNSLIB) $(LAPACKLIB) $(METISLIB)

# include librairies.make

LIBNAMES = \
       libt_main   \
       libt_xio    \
       libt_zone   \
       libt_mgrid  \
       libt_exch   \
       libt_param  \
       libt_vortex \
       libt_eqns   \
       libt_eqkdif \
       libt_udf    \
       libt_mesh   \
       libt_pio    \
       libt_math   \
       libt_modcom

LIBS = $(addsuffix .$(LIBSTA),$(LIBNAMES))

D_LIBS = $(LIBS:%=$(PRJLIBDIR)/%)

####### Program names

PRGNAME = $(PRGBASE).$(PRGEXT).$(optext)

PRGLINK = $(PRGBASE)-$(PRGEXT)

####### VPATH

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

MAIN = $(PRJOBJDIR)/main.o # dependencies in MAIN/depends.make

####### Build rules

$(PRG.TARGETS): $(PRJOBJDIR) $(PRJLIBDIR) $(MAKEFILE)

$(PRG.TARGETS): $(D_LIBS) $(PRGLINK)

$(PRGLINK): $(MAIN) $(D_LIBS) $(MAKEFILE)
	@echo "----------------------------------------------------------------"
	@echo "--- CREATING $@ ---"
	@echo "external Libraries: $(PRJLIBEXT) $(EXTLIBS)" $(EXTMPILIB)
	@echo "TYPHON   Libraries: $(LIBS)"
	(cd $(PRJLIBDIR) ; $(LINKER) $(LINKFB) ../$(MAIN) $(LIBS) $(PRJLIBEXT) $(EXTLIBS) $(EXTMPILIB) -o ../$(PRGNAME))
	@echo "--- CREATION OF $@ DONE ---"
	@echo "--- LINKING $(PRGLINK) ---"
	@echo "--- link $(PRGLINK) to $(PRGNAME) ---"
	@ln -fs $(PRGNAME) $(PRGLINK)
	@echo "--- LINK OF $(PRGLINK) DONE ---"

clean:
	@echo
	@echo "clean targets:"
	@echo "  clean.all       -> clean seq, omp & mpi targets"
	@echo "  clean.seq       -> clean seq    targets"
	@echo "  clean.omp       -> clean openmp targets"
	@echo "  clean.mpi       -> clean mpi    targets"

clean.all: $(clean.TARGETS)

$(clean.TARGETS): clean.%:
	rm -f $(PRGBASE).$*.$(optext) $(PRGBASE)-$*
	rm -f $(PRJOBJDIR)/*.o
	rm -f $(D_LIBS)
	rm -f $(PRJINCDIR)/*.$(MODEXT)
	rm -f *.$(MODEXT)

####### Dependencies

$(PRGNAME) $(PRGLINK): $(MAKEFILE)

# Module dependencies

