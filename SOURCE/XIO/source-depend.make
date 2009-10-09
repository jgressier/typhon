############################################################
##   XIO library compilation

LDIR := XIO

####### Files

# Library
XIO_LIB = $(PRJLIB)/libt_xio.a

# Modules
XIO_MOD = REPRISE.$(MOD)

# Objects
XIO_OBJ = $(XIO_MOD:.$(MOD)=.o)  \
          comp_flux.o            \
          output_tec_str.o       \
          output_tec_ust.o       \
          output_tec_ust_ctr.o   \
          output_tecplot.o       \
          output_zone.o          \
          outputzone_close.o     \
          outputzone_sol.o       \
          outputzone_open.o      \
          outputzone_ustmesh.o   \
          readtyphmsh_dom.o      \
          readtyphmshfile.o      \
          writevtk_scal.o        \
          writevtk_sol.o         \
          writevtk_ustmesh.o     \
          writevtk_vect.o        \

D_XIO_OBJ = $(XIO_OBJ:%=$(PRJOBJ)/%)

D_XIO_SRC = $(XIO_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(XIO_LIB)

$(XIO_LIB): $(D_XIO_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(XIO_LIB)
	@touch $(XIO_LIB) ; rm $(XIO_LIB)
	@$(AR) ruv $(XIO_LIB) $(D_XIO_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(XIO_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(XIO_LIB) created
	@echo ---------------------------------------------------------------

XIO_clean:
	-rm $(XIO_LIB) $(D_XIO_OBJ) $(XIO_MOD) XIO/depends.make


####### Dependencies

XIO/depends.make: $(D_XIO_SRC)
	(cd XIO ; ../$(MAKEDEPENDS))

include XIO/depends.make


