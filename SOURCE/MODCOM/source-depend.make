############################################################
##   MODCOM library compilation

LDIR := MODCOM

####### Files

# Library
MODCOM_LIB = $(PRJLIB)/libt_modcom.a

# Modules
MODCOM_MOD = COMMTAG.$(MOD)          \
             CONNECTIVITY.$(MOD)     \
             CONNECT_CSR.$(MOD)      \
             GENLIB.$(MOD)           \
             GEO2D.$(MOD)            \
             LAPACK.$(MOD)           \
             LIBSORT.$(MOD)          \
             MODINFO.$(MOD)          \
             STRING.$(MOD)           \
             TYPHMAKE.$(MOD)         \
             VARCOM.$(MOD)

# Objects
MODCOM_OBJ = $(MODCOM_MOD:.$(MOD)=.o) # \

D_MODCOM_OBJ = $(MODCOM_OBJ:%=$(PRJOBJ)/%)

D_MODCOM_SRC := $(MODCOM_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

all: $(MODCOM_LIB)

$(MODCOM_LIB): $(D_MODCOM_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(MODCOM_LIB)
	@touch $(MODCOM_LIB) ; rm $(MODCOM_LIB)
	@$(AR) ruv $(MODCOM_LIB) $(D_MODCOM_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(MODCOM_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(MODCOM_LIB) created
	@echo ---------------------------------------------------------------

MODCOM_clean:
	-rm $(MODCOM_LIB) $(D_MODCOM_OBJ) $(MODCOM_MOD) MODCOM/depends.make


####### Dependencies

MODCOM/depends.make: $(D_MODCOM_SRC)
	(cd MODCOM ; ../$(MAKEDEPENDS))

include MODCOM/depends.make


