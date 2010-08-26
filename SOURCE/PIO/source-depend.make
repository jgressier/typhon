############################################################
##   PIO library compilation

LDIR := PIO

####### Files

# Library
PIO_LIB = $(PRJLIBDIR)/libt_pio.a

# Modules
PIO_MOD = OUTPUT.$(MODEXT)   \
          RPM.$(MODEXT)      \

# Objects
PIO_OBJ = $(PIO_MOD:.$(MODEXT)=.o)  \
          rpm_output.o

libt_pio.objects = $(PIO_OBJ:%=$(PRJOBJDIR)/%)
libt_pio.target: $(libt_pio.objects)

D_PIO_SRC = $(PIO_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

PIO_clean:
	-rm $(PIO_LIB) $(libt_pio.objects) $(PIO_MOD) PIO/depends.make


####### Dependencies

PIO/depends.make: $(D_PIO_SRC)
	$(MAKEDEPENDS) PIO

include PIO/depends.make


