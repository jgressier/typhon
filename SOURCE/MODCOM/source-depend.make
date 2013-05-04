############################################################
##   MODCOM library compilation

LDIR := MODCOM

####### Files

# Library
MODCOM_LIB = $(PRJLIBDIR)/libt_modcom.a

# Modules
MODCOM_MOD = COMMTAG.$(MODEXT)          \
             GENLIB.$(MODEXT)           \
             LAPACK.$(MODEXT)           \
             MODINFO.$(MODEXT)          \
             TYPHMAKE.$(MODEXT)         \
             VARCOM.$(MODEXT)

# Objects
MODCOM_OBJ = $(MODCOM_MOD:.$(MODEXT)=.o) # \

libt_modcom.objects = $(MODCOM_OBJ:%=$(PRJOBJDIR)/%)
libt_modcom.target: $(libt_modcom.objects)

D_MODCOM_SRC := $(MODCOM_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

MODCOM_clean:
	-rm $(MODCOM_LIB) $(libt_modcom.objects) $(MODCOM_MOD) MODCOM/depends.make


####### Dependencies

MODCOM/depends.make: $(D_MODCOM_SRC)
	$(MAKEDEPENDS) MODCOM

include MODCOM/depends.make


