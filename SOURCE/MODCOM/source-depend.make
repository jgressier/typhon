############################################################
##   Compilation de la librairie MODCOM

LDIR := MODCOM

####### Files

MODCOM_LIB = $(PRJLIB)/libt_modcom.a

MODCOM_MOD = CONNECTIVITY.$(MOD) \
             TYPHMAKE.$(MOD)     \
             VARCOM.$(MOD) 

MODCOM_OBJ = $(MODCOM_MOD:.$(MOD)=.o) # \

D_MODCOM_OBJ = $(MODCOM_OBJ:%=$(PRJOBJ)/%)

D_MODCOM_SRC := $(MODCOM_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(MODCOM_LIB)

$(MODCOM_LIB): $(D_MODCOM_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(MODCOM_LIB)
	@touch $(MODCOM_LIB) ; rm $(MODCOM_LIB)
	@$(AR) ruv $(MODCOM_LIB) $(D_MODCOM_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(MODCOM_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(MODCOM_LIB) créée
	@echo ---------------------------------------------------------------

MODCOM_clean:
	-rm  $(MODCOM_LIB) $(D_MODCOM_OBJ) $(MODCOM_MOD)

####### Dependencies

MODCOM/depends.make: $(D_MODCOM_SRC)
	(cd MODCOM ; ../$(MAKEDEPENDS))

include MODCOM/depends.make




