############################################################
##   Compilation de la librairie PIO

LDIR = PIO

####### Files

PIO_LIB = libt_pio.a

PIO_MOD = OUTPUT.$(MOD)   \
          RPM.$(MOD)      \
          STRING.$(MOD)

PIO_OBJ = $(PIO_MOD:.$(MOD)=.o)  \
          erreur.o               \
          rpm_output.o

D_PIO_OBJ = $(PIO_OBJ:%=$(PRJOBJ)/%)

D_PIO_SRC = $(PIO_OBJ:%.o=$(LDIR)/%.f90)

####### Build rules

#all: $(PRJLIB)/$(PIO_LIB)

$(PRJLIB)/$(PIO_LIB): $(PIO_LIB)
	@echo \* Copie de la librairie $(PIO_LIB)
	@cp $(PIO_LIB) $(PRJLIB)

$(PIO_LIB): $(D_PIO_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(PIO_LIB)
	@touch $(PIO_LIB) ; rm $(PIO_LIB)
	@$(AR) ruv $(PIO_LIB) $(D_PIO_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(PIO_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(PIO_LIB) créée
	@echo ---------------------------------------------------------------

PIO_clean:
	-rm  $(PIO_LIB) $(D_PIO_OBJ) $(PIO_MOD)

####### Dependencies


PIO/depends.make: $(D_PIO_SRC)
	(cd PIO ; ../$(MAKEDEPENDS))

include PIO/depends.make



