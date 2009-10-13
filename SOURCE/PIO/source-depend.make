############################################################
##   PIO library compilation

LDIR := PIO

####### Files

# Library
PIO_LIB = $(PRJLIB)/libt_pio.a

# Modules
PIO_MOD = IO_UNIT.$(MOD)  \
          OUTPUT.$(MOD)   \
          RPM.$(MOD)      \

# Objects
PIO_OBJ = $(PIO_MOD:.$(MOD)=.o)  \
          rpm_output.o

D_PIO_OBJ = $(PIO_OBJ:%=$(PRJOBJ)/%)

D_PIO_SRC = $(PIO_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(PIO_LIB)

$(PIO_LIB): $(D_PIO_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(PIO_LIB)
	@rm -f $(PIO_LIB)
	@$(AR) ruv $(PIO_LIB) $(D_PIO_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(PIO_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(PIO_LIB) created
	@echo ---------------------------------------------------------------

PIO_clean:
	-rm $(PIO_LIB) $(D_PIO_OBJ) $(PIO_MOD) PIO/depends.make


####### Dependencies

PIO/depends.make: $(D_PIO_SRC)
	(cd PIO ; ../$(MAKEDEPENDS))

include PIO/depends.make


