############################################################
##   XIO library compilation

LDIR := XIO

####### Files

# Library
XIO_LIB = $(PRJLIBDIR)/libt_xio.a

# Modules
XIO_MOD = REPRISE.$(MODEXT)

# Objects
XIO_OBJ = $(XIO_MOD:.$(MODEXT)=.o)  \
          comp_flux.o            \
          output_tec_ust.o       \
          output_tec_ust_ctr.o   \
          output_tec_ust_boco.o  \
          output_tecplot.o       \
          output_zone.o          \
          outputzone_close.o     \
          outputzone_sol.o       \
          outputzone_open.o      \
          outputzone_ustmesh.o   \

libt_xio.objects = $(XIO_OBJ:%=$(PRJOBJDIR)/%)
libt_xio.target: $(libt_xio.objects)

D_XIO_SRC = $(XIO_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

XIO_clean:
	-rm $(XIO_LIB) $(libt_xio.objects) $(XIO_MOD) XIO/depends.make


####### Dependencies

XIO/depends.make: $(D_XIO_SRC)
	$(MAKEDEPENDS) XIO

include XIO/depends.make


